detectionData <- detectionsAttributesFlows

getMovementsFunction <- function(detectionData) {
  
  detectionsFIrstLast <- detectionData %>%
    group_by(dec_tag, DetectionDate) %>%
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
    mutate(
      # movement = case_when(str_detect(antennaName, c("Downstream")) & str_detect(lag(antennaName), c("Upstream")) ~ "Downstream Movement", 
      #                           str_detect(antennaName, c("Upstream")) & str_detect(lag(antennaName), c("Downstream")) ~ "Upstream Movement", 
      #                           antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
      #                           antennaName == lag(antennaName) ~ "No Movement",
      #                           is.na(lag(antennaName)) ~ "First Antenna Detection",
      #                           TRUE ~ NA), 
           #status of if a fish is in the study area or not
           #if there is no previous antena name, it's the first of the detections and the fish is inside the study area
           `Study Area Status` = case_when(is.na(lag(antennaName)) | str_detect(antennaName, c("Upstream")) ~ "Inside study area", 
                             str_detect(antennaName, c("Downstream")) ~"Outside study area",
                             antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
                             TRUE ~ NA
           )
           #detectionDate = date(detected)
           # long = st_coordinates(detectionsSF)[row_number(),1], 
           # lat = st_coordinates(detectionsSF)[row_number(),2]
    )
  
  ###For "movements" we're looking at poplation levels of whether or not a fish in the the study area or not at the last detection of the day
  statusLastOfDay <- dailyMovementsTableAll %>%
    ungroup() %>%
    filter(first_last == "Last_of_day")
  # dailyMovementsTablemoveFirstLast <- dailyMovementsTableAll %>%
  #   #separate to main array and cow creek
  #   mutate(array = if_else(antennaName %in% c("Uncompahgre River Antenna Downstream", "Uncompahgre River Antenna Upstream"), "Main Array", "Cow Creek")) %>%
  #   #remove detections during the day ebtween the first and last detections
  #   filter(first_last != "0") %>%
  #   distinct(DetectionDate, dec_tag, array, first_last, .keep_all = TRUE) %>%
  #   ungroup()
  
  
  return(statusLastOfDay)
}