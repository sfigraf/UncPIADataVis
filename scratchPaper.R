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
detectionsSF <- detections %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  st_as_sf()
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
  #filter(dec_tag == "3DD.0078E38638") %>% 0078E385C1
  mutate(movement = case_when(str_detect(antennaName, c("Downstream")) & str_detect(lag(antennaName), c("Upstream")) ~ "Downstream Movement", 
                              str_detect(antennaName, c("Upstream")) & str_detect(lag(antennaName), c("Downstream")) ~ "Upstream Movement", 
                              antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
                              antennaName == lag(antennaName) ~ "No Movement",
                              TRUE ~ NA), 
         Date = date(detected), 
         long = st_coordinates(detectionsSF)[row_number(),1], 
         lat = st_coordinates(detectionsSF)[row_number(),2]
  ) %>%
  st_drop_geometry()

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
detectionData <- detectionsSF
detectionDatafiltered <- detectionData  %>% 
  filter(
    Date >= "2025-11-11" & Date <= "2025-12-16"),
    antennaName %in% c(input$arrayPicker),
    antenna %in% c(input$picker7),
    SPP %in% c(input$picker10),
    `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
    
  ) %>%
  arrange(detected)
