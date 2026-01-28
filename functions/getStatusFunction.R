#detectionData <- detectionsAttributesFlows
#funciton gets the "Status" of a fish based off the last array they ended the day on
#as of 1/5 2025 it's applied to all data and the static file is used in the app

getStatusFunction <- function(detectionData, studyStartDate) {
  
  #remove duplicate detection rows: most tags don't have this but one tag (12/29/2025) was detected same timestamp on dif antennas 
  #throws off first/last if timestamps were both first or last of the day
  #989.00104049961897 for example
  #specific antenna detected doesn't particularly matter when were calculating movements/how many fish stayed above/below study area
  detectionDataDistinct <- detectionData %>%
    distinct(dec_tag, detected, antennaName, .keep_all = TRUE)
  
  # see all isntances here 
  # removedRows <- detectionData %>%
  #   anti_join(detectionDataDistinct)
  
  detectionsFIrstLast <- detectionDataDistinct %>%
    group_by(dec_tag, DetectionDate) %>%
    arrange(detected) %>%
    #we want to prioritize last_ofDay so put that first. if there's a single array hit that day this way it will register "last of day"
    #example 989.002028177009
    mutate(first_last = case_when(detected == max(detected) ~ "Last_of_day",
                                  detected == min(detected) ~ "First_of_day",
                                  detected != min(detected) & detected != max(detected) ~ "0")) %>%
    ungroup()
  
  dailyMovementsTableAll <- detectionsFIrstLast %>%
    group_by(dec_tag) %>%
    arrange(detected) %>%
    #989.002028176951
    #filter(dec_tag == "3DD.0078E38638") %>% 0078E385C1
    mutate(
      
      #status of if a fish is in the study area or not
      #if there is no previous antena name, it's the first of the detections and the fish is inside the study area
      `Study Area Status` = case_when(is.na(lag(antennaName)) | str_detect(antennaName, c("Upstream")) ~ "Inside study area", 
                                      str_detect(antennaName, c("Downstream")) ~"Outside study area",
                                      antennaName == "Cow Creek Antenna" ~ "In Cow Creek",
                                      TRUE ~ NA
      )
      
      # long = st_coordinates(detectionsSF)[row_number(),1], 
      # lat = st_coordinates(detectionsSF)[row_number(),2]
    )
  #find first detection after release to get pre study status
  #MAYBE CHANGE TO JOIN ON NEWTAG ONCE RELEASE DATA IS CLEANER
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
  
  #gets all tags, especially ones not detected yet with antennas
  allTagsStatusDf <- statusLastOfDay %>%
    right_join(Unc_Tag_Releases1[,c("Dec Tag #")], by = c("newTag" = "Dec Tag #"))
  #for tags not detected yet on antennas, we assume they're still within the study area
  #this will change if a tag is detected first on the downstream antenna; will get caught in the "preStudyStatus" column
  
  #also create "StatusDate" column based on filtered data and if a fish has already bene detected that day; will be used to make a sequence
  #studyStartDate a global variable, should probbaly pass this to the function
  minDate <- if_else(min(date(allTagsStatusDf$detected), na.rm = TRUE) >= studyStartDate, min(date(allTagsStatusDf$detected), na.rm = TRUE), studyStartDate)
  allTagsStatusDf2 <- allTagsStatusDf %>%
    mutate(`Study Area Status` = if_else(is.na(`Study Area Status`), "Inside study area", `Study Area Status`), 
           StatusDate = if_else(!is.na(DetectionDate), DetectionDate, minDate)
           #StatusDate = coalesce(DetectionDate, `Release Date.y`)
    )
  allTagsStatusDfCompleted <- allTagsStatusDf2 %>%
    #group_by(newTag) %>%
    #ungroup() %>%
    #arrange(StatusDate) %>%
    #first arg is group so group by new tag, then next arg is column you have to fill in. Must be present in data
    tidyr::complete(newTag, StatusDate = seq.Date(
      from = min(StatusDate),
      to   = max(StatusDate),
      by   = "day"
    )
    )
  #fill in missing values
  
  allTagsStatusDfFilled <- allTagsStatusDfCompleted %>%
    group_by(newTag) %>%
    arrange(StatusDate) %>%
    #ensures that missing values get changed 
    tidyr::fill(`Study Area Status`, .direction = "down") %>%
    tidyr::fill(preStudyStatus, .direction = "up") %>%
    #fill in rest of attribute info for filtering purposes
    #tidyr::fill(`TL 1st Enc. (mm)`, `SPP`, `Release Date`, antennaName, antenna, Flow, .direction = "updown") %>%
    #for the sutdy area that didn't have a previous one to fill down, it's at the beginning of the study so use values from "preStudyStatus" 
    #IF IT WAS OUTSIDE THE STUDY AREA AND CAME BACK IN ITS FIRST DETECTION WILL BE THE DOWNSTREAM ARRAY
    #ie tag 989.001040499618
    #replace_na might be cleaner and faster but this is more descriptive
    mutate(`Study Area Status` = if_else(is.na(`Study Area Status`), preStudyStatus, `Study Area Status`)) %>%
    ungroup()
  
  #join back with release file to get all attribute info relevant for filtering
  #shouldn't get a warning message when all duplicate tag entries are sorted out
  allTagsStatusDfFilledAttributes <- allTagsStatusDfFilled %>%
    left_join(Unc_Tag_Releases1, by = c("newTag" = "Dec Tag #")) %>%
    left_join(USGSFlows$USGSDataDaily, by = c("StatusDate" = "Date"))
  
  allTagsStatusDfFilledAttributesCleaned <- allTagsStatusDfFilledAttributes %>%
    mutate(Flow = coalesce(Flow.x, Flow.y), 
           `TL 1st Enc. (mm)` = coalesce(`TL 1st Enc. (mm).x`, `TL 1st Enc. (mm).y`), 
           Species = coalesce(Species.x, Species.y), 
           `Release Date` = coalesce(`Release Date.x`, `Release Date.y`)) %>%
    select(StatusDate, names(detectionData), `preStudyStatus`, `Study Area Status`) %>%
    #get rid of unnecessary rows, which only occur in this function bc of duplicate tag entries (like the many-many join relationship). Once data is clean this shouldn;t be needed
    distinct(.keep_all = TRUE)
  
  return(allTagsStatusDfFilledAttributesCleaned)
}

