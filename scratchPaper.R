#notes
library(dataRetrieval)

antennas <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326)
centerCoords <- st_coordinates(st_centroid(st_union(antennas)))
fitBounds(antennaMetadata$long[1], antennaMetadata$lat[1], antennaMetadata$long[nrow(antennaMetadata)], 
          antennaMetadata$lat[nrow(antennaMetadata)])

leaflet(detectionsSF) %>%
  addTiles() %>%
  addAwesomeMarkers()
################
# in this one there is 989.00103062018604
uniqueDetected <- sort(unique(detections$dec_tag))
# in this one there is 989.001030620186
uniqueReleased <- unique(Unc_Tag_Releases1$`Full Tag`)

detections1 <- detections %>%
  mutate(newTag = if_else(str_length(dec_tag) == 18, substr(dec_tag, 1, nchar(dec_tag) - 2), dec_tag))
detectionsSF <- detections1 %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  left_join(Unc_Tag_Releases1, by = c("newTag" = "Full Tag")) %>%
  st_as_sf()

NAs <- detectionsSF %>%
  st_drop_geometry() %>%
  filter(is.na(SPP)) %>%
  distinct(newTag, .keep_all = TRUE)

Unc_Tag_Releases2 <- Unc_Tag_Releases1 %>%
  mutate(numDigits = str_length(`Full Tag`)) %>%
  count(numDigits)
detectionsSFSumarized <- detectionsSF %>%
  st_drop_geometry() %>%
  mutate(numDigits = str_length(`dec_tag`)) %>%
  distinct(dec_tag, .keep_all = TRUE) %>%
  count(numDigits)


#########
detectionsSF <- detections1 %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  left_join(Unc_Tag_Releases1, by = c("newTag" = "Full Tag")) 

detectionsFIrstLast <- detectionsSF %>%
  group_by(dec_tag, date(detected)) %>%
  arrange(detected) %>%
  mutate(first_last = case_when(detected == min(detected) ~ "First_of_day",
                                detected == max(detected) ~ "Last_of_day",
                                detected != min(detected) & detected != max(detected) ~ "0")) %>%
  ungroup()

dailyMovementsTableAll <- detectionsFIrstLast %>%
  group_by(dec_tag) %>%
  arrange(detected) %>%
  #989.002028176951
  #filter(dec_tag == "3DD.0078E38638") %>% 0078E385C1
  mutate(movement = case_when(str_detect(antennaName, c("Downstream")) & str_detect(lag(antennaName), c("Upstream")) ~ "Downstream Movement", 
                              str_detect(antennaName, c("Upstream")) & str_detect(lag(antennaName), c("Downstream")) ~ "Upstream Movement", 
                              antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
                              antennaName == lag(antennaName) ~ "No Movement",
                              TRUE ~ NA), 
         detectionDate = date(detected)
         # long = st_coordinates(detectionsSF)[row_number(),1], 
         # lat = st_coordinates(detectionsSF)[row_number(),2]
  ) #%>%
  #st_drop_geometry()

dailyMovementsTablemoveOnly <- dailyMovementsTableAll %>%
  distinct(Date, dec_tag, antennaName, movement, .keep_all = TRUE)
dailyMovementsTablemoveFirstLastMovement <- dailyMovementsTableAll %>%
  distinct(Date, dec_tag, antennaName, first_last, movement, .keep_all = TRUE)

dailyMovementsTablemoveFirstLast <- dailyMovementsTableAll %>%
  #separate to main array and cow creek
  mutate(array = if_else(antennaName %in% c("Uncompahgre River Antenna Downstream", "Uncompahgre River Antenna Upstream"), "Main Array", "Cow Creek")) %>%
  #remove detections during the day ebtween the first and last detections
  filter(first_last != "0") %>%
  distinct(Date, dec_tag, array, first_last, .keep_all = TRUE)

x <- dailyMovementsTablemoveFirstLast %>%
  anti_join(dailyMovementsTablemoveOnly)

######filters 
# detectionData <- detectionsSF
# detectionDatafiltered <- detectionData  %>% 
#   filter(
#     Date >= "2025-11-11" & Date <= "2025-12-16"),
#     antennaName %in% c(input$arrayPicker),
#     antenna %in% c(input$picker7),
#     SPP %in% c(input$picker10),
#     `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
#     
#   ) %>%
#   arrange(detected)

###greaphing 

dailyMovementsTablemoveFirstLast <- getMovementsFunction(detectionsAttributesFlows)

movementCounts <- dailyMovementsTablemoveFirstLast %>%
  count(movement, DetectionDate)

line_color = I("#87CEEB")
nameOfLine = "USGS Discharge"
input <- list("YaxisSelect" = "Detections" )

if(input$YaxisSelect == "Detections"){
  movYaxis = "y1"
  envYaxis = "y2"
  primaryYaxisName = "Detection Data (Daily Counts)"
  SecondaryYaxisName = "Discharge"
  
} else{
  movYaxis = "y2"
  envYaxis = "y1"
  primaryYaxisName = "Discharge"
  SecondaryYaxisName = "Detection Data (Daily Counts)"
}

plot_ly() %>%
  # add_trace(data = dailyMovementsTablemoveFirstLast, x = ~DetectionDate,
  #           y = ~Flow,
  #           name = nameOfLine,
  #           color = line_color, 
  #           type = "scatter",
  #           yaxis = envYaxis,
  #           connectgaps = TRUE,
  #           mode = "lines"
  #           #colors = allColors
  # ) %>%
  add_trace(data = movementCounts, x = ~DetectionDate, y = ~n,
            yaxis = movYaxis,
            color = ~movement,
            #colors = allColors,
            hoverinfo = "text",
            text = ~paste('Date: ', as.character(DetectionDate), '<br>Number of movements: ', n),
            type = 'bar') %>%
  layout(legend = list(x = 1.05, y = 1),
         barmode = "overlay",
         xaxis = list(title = "Date"),
         yaxis = list(title = primaryYaxisName, side = "left", showgrid = FALSE),
         yaxis2 = list(title = SecondaryYaxisName, side = "right", overlaying = "y",
                       showgrid = FALSE))
