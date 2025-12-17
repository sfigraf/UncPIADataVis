#notes
library(dataRetrieval)

antennas <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326)
centerCoords <- st_coordinates(st_centroid(st_union(antennas)))
fitBounds(antennaMetadata$long[1], antennaMetadata$lat[1], antennaMetadata$long[nrow(antennaMetadata)], 
          antennaMetadata$lat[nrow(antennaMetadata)])

leaflet(detectionsSF) %>%
  addTiles() %>%
  addAwesomeMarkers()
#########
detectionsSF <- detections %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  st_as_sf()
detectionsFIrstLast <- detectionsSF %>%
  group_by(tag, date(detected)) %>%
  arrange(detected) %>%
  mutate(first_last = case_when(detected == min(detected) ~ "First_of_day",
                                detected == max(detected) ~ "Last_of_day",
                                detected != min(detected) & detected != max(detected) ~ "0")) %>%
  ungroup()

dailyMovementsTableAll <- detectionsFIrstLast %>%
  group_by(tag) %>%
  arrange(detected) %>%
  #filter(tag == "3DD.0078E38638") %>% 0078E385C1
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
  distinct(Date, tag, antennaName, movement, .keep_all = TRUE)
dailyMovementsTablemoveFirstLastMovement <- dailyMovementsTableAll %>%
  distinct(Date, tag, antennaName, first_last, movement, .keep_all = TRUE)

dailyMovementsTablemoveFirstLast <- dailyMovementsTableAll %>%
  #separate to main array and cow creek
  mutate(array = if_else(antennaName %in% c("Uncompahgre River Antenna Downstream", "Uncompahgre River Antenna Upstream"), "Main Array", "Cow Creek")) %>%
  #remove detections during the day ebtween the first and last detections
  filter(first_last != "0") %>%
  distinct(Date, tag, array, first_last, .keep_all = TRUE)

x <- dailyMovementsTablemoveFirstLast %>%
  anti_join(dailyMovementsTablemoveOnly)
