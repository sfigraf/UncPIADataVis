####find how many fish at a given time are inside/outside sutdy area
detectionData <- detectionsAttributesFlows
statusDf <- getMovementsFunction(detectionsAttributesFlows)
studyStartDate <- min(date(detections$detected))

#gets all tags, especially ones not detected yet with antennas
allTagsStatusDf <- statusDf %>%
  right_join(Unc_Tag_Releases1[,c("Full Tag", "Release Date")], by = c("newTag" = "Full Tag"))
#for tags not detected yet on antennas, we assume they're still within the study area
#this will change if a tag is detected first on the downstream antenna; will get caught in the "preStudyStatus" column
x1 <- allTagsStatusDf %>%
  mutate(`Study Area Status` = if_else(is.na(`Study Area Status`), "Inside study area", `Study Area Status`), 
         StatusDate = if_else(!is.na(DetectionDate), DetectionDate, studyStartDate)
         #StatusDate = coalesce(DetectionDate, `Release Date.y`)
  ) #%>%
#st_drop_geometry()
# now complete dailyt status for each tag
#can do with Data.table 
# library(data.table)
# setDT(x1)
# 
# # This performs the sequence generation for each tag individually
# df_completed <- x1[, .(day = seq(studyStartDate, max(x1$StatusDate), by = "day")), 
#                    by = newTag]
# detach("package:data.table", unload=TRUE)
x2 <- x1 %>%
  #group_by(newTag) %>%
  #ungroup() %>%
  #arrange(StatusDate) %>%
  #first arg is group so group by new tag, then next arg is column you have to fill in. Must be present in data
  tidyr::complete(newTag, StatusDate = seq.Date(
    from = min(StatusDate),
    to   = max(x1$StatusDate),
    by   = "day"
  )
  )

#fill in missing values

x3 <- x2 %>%
  group_by(newTag) %>%
  arrange(StatusDate) %>%
  fill(`Study Area Status`, .direction = "down") #%>%
  #for the sutdy area that didn't have a previous one to fill down, it's at the beginning of the study so it's assumed right now that they are inside the study area
  #NEED TO CODE IN IF A FISH WAS RELEASED BEFORE THE STUDY, THEN IF IT WAS OUTSIDE THE STUDY AREA AND CAME BACK IN ITS FIRST DETECTION WILL BE THE DOWNSTREAM ARRAY
  
  mutate(`Study Area Status` = if_else(is.na(`Study Area Status`), "Inside study area", `Study Area Status`)) %>%
  ungroup()

proportionCounts <- x3 %>%
  group_by(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) %>%
  summarize(n = n())
#should add up to the total number of rows in 
totals <- proportionCounts %>%
  group_by(DetectionDate) %>%
  summarise(total = sum(n))



# uniqueTags <- x3 %>%
#   distinct(newTag) %>%
#   count(newTag) %>%
#   mutate(nDigits = str_length(newTag))

#tag that 989.001040500063
x4 <- x3 %>%
  filter(is.na(`Study Area Status`))
