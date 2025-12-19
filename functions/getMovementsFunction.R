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
    mutate(movement = case_when(str_detect(antennaName, c("Downstream")) & str_detect(lag(antennaName), c("Upstream")) ~ "Downstream Movement", 
                                str_detect(antennaName, c("Upstream")) & str_detect(lag(antennaName), c("Downstream")) ~ "Upstream Movement", 
                                antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
                                antennaName == lag(antennaName) ~ "No Movement",
                                TRUE ~ NA)
           #detectionDate = date(detected)
           # long = st_coordinates(detectionsSF)[row_number(),1], 
           # lat = st_coordinates(detectionsSF)[row_number(),2]
    )
  
  ###CURRENTLY USING "condense it to just the first and last detections of the day and separate the antennas to "main array" vs "cow creek"."
  #COME BACK TO IT WHEN DAN EMAILS ME BACK
  dailyMovementsTablemoveFirstLast <- dailyMovementsTableAll %>%
    #separate to main array and cow creek
    mutate(array = if_else(antennaName %in% c("Uncompahgre River Antenna Downstream", "Uncompahgre River Antenna Upstream"), "Main Array", "Cow Creek")) %>%
    #remove detections during the day ebtween the first and last detections
    filter(first_last != "0") %>%
    distinct(DetectionDate, dec_tag, array, first_last, .keep_all = TRUE) %>%
    ungroup()
  
  
  return(dailyMovementsTablemoveFirstLast)
}