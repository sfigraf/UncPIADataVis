#detectionData <- detectionsAttributesFlows

getMovementsFunction <- function(detectionData) {
  
  #remove duplicate detection rows: most tags don't have this but one tag (12/29/2025) was detected same timestamp on dif antennas 
  #throws off first/last if timestamps were both first or last of the day
  #989.00104049961897 for example
  #specific antenna detected doesn't particlaruly matter when were calculating movemnts/how many fish stayed above/below study area
  detectionDataDistinct <- detectionData %>%
    distinct(dec_tag, detected, antennaName, .keep_all = TRUE)
  
  # see all isntances here 
  # removedRows <- detectionData %>%
  #   anti_join(detectionDataDistinct)
  detectionsFIrstLast <- detectionDataDistinct %>%
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
  #find first detection after release
  preStudyTagStatus <- dailyMovementsTableAll %>%
    group_by(dec_tag) %>%
    arrange(detected) %>%
    #if the fish has no previous antenna assigned and the first detection is donwstream array or cow creek, then it was outside the study area before the start of the study
    mutate(preStudyStatus = if_else(is.na(lag(antennaName)) & (str_detect(antennaName, c("Downstream")) | str_detect(antennaName, c("Cow Creek Antenna"))), "Outside study area", "Inside study area")) %>%
    filter(detected == first(detected))
  
  
  
  ###For "movements" we're looking at poplation levels of whether or not a fish in the the study area or not at the last detection of the day
  statusLastOfDay <- dailyMovementsTableAll %>%
    ungroup() %>%
    filter(first_last == "Last_of_day") %>%
    left_join(preStudyTagStatus[,c("dec_tag", "preStudyStatus")], by = "dec_tag")
  # dailyMovementsTablemoveFirstLast <- dailyMovementsTableAll %>%
  #   #separate to main array and cow creek
  #   mutate(array = if_else(antennaName %in% c("Uncompahgre River Antenna Downstream", "Uncompahgre River Antenna Upstream"), "Main Array", "Cow Creek")) %>%
  #   #remove detections during the day ebtween the first and last detections
  #   filter(first_last != "0") %>%
  #   distinct(DetectionDate, dec_tag, array, first_last, .keep_all = TRUE) %>%
  #   ungroup()
  
  
  return(statusLastOfDay)
}

#difs 
# statusLastOfDayCompare <- statusLastOfDay %>%
#   count(dec_tag)
# statusDfCounts <- statusDf %>%
#   count(dec_tag)
#   #anti_join(statusLastOfDayCompare)
# difs <- statusLastOfDayCompare %>%
#   anti_join(statusDfCounts)
