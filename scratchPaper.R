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
detectionsAttributesFlows
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
                              is.na(lag(antennaName)) ~ "First Antenna Detection",
                              TRUE ~ NA), 
         #states of if a fish is in the study area or not
         #if there is no previous antena name, it's the first of the detections and the fish is inside the study area
         State = case_when(is.na(lag(antennaName)) | str_detect(antennaName, c("Upstream")) ~ "Inside study area", 
                           str_detect(antennaName, c("Downstream")) ~"Outside study area",
                           antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
                           TRUE ~ NA
                           #if the fish's last antenna was US antenna, it's inside the study area
                           )
         #detectionDate = date(detected)
         # long = st_coordinates(detectionsSF)[row_number(),1], 
         # lat = st_coordinates(detectionsSF)[row_number(),2]
  )



filteredData <- dailyMovementsTableAll %>%
  ungroup() %>%
  filter(first_last == "Last_of_day")
filteredDataAll <- filteredData %>%
  filter(DetectionDate == as.Date("2025-11-15"), 
         State == "Outside study area")
filteredDataDistinct <- filteredData %>%
  distinct(newTag, DetectionDate, .keep_all = T) %>%
  filter(DetectionDate == as.Date("2025-11-15"), 
         State == "Outside study area")

tagDifs <- anti_join(filteredDataAll, filteredDataDistinct, by = "newTag" )

studyAreaCounts <- filteredData %>%
  count(DetectionDate, State)
  #get number of fish that had a downstream movement on the end of the day (aka ended the day outside the study area)
  #compare that to total number of fish tagged

###tags with more than 1 dec_tag
morethan1dec_tag <- detectionsAttributesFlows %>%
  distinct(dec_tag, newTag) %>%
  count(`Release File tag entry` = newTag, name = "Number of dec_tag Entries") %>%
  filter(`Number of dec_tag Entries` > 1)



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


# Study Area Proportion Notes ---------------------------------------------


#how many fish are in the study area vs not on avery given day
#start with releasedTags
####find how many fish at a given time are inside/outside sutdy area
detectionData <- detectionsAttributesFlows
statusDf <- getMovementsFunction(detectionsAttributesFlows)
studyStartDate <- min(date(detections$detected))

#gets all tags, especially ones not detected yet with antennas
allTagsStatusDf <- statusDf %>%
  right_join(Unc_Tag_Releases1[,c("Full Tag", "Release Date")], by = c("newTag" = "Full Tag"))
#for tags not detected yet on antennas, we assume they're still within the study area
#this will change if a tag is detected first on the downstream antenna; will get caught in the "preStudyStatus" column
allTagsStatusDf2 <- allTagsStatusDf %>%
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
allTagsStatusDfCompleted <- allTagsStatusDf2 %>%
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

allTagsStatusDfFilled <- allTagsStatusDfCompleted %>%
  group_by(newTag) %>%
  arrange(StatusDate) %>%
  #ensures that missing values get changed 
  tidyr::fill(`Study Area Status`, .direction = "down") %>%
  tidyr::fill(preStudyStatus, .direction = "up") %>%
  #for the sutdy area that didn't have a previous one to fill down, it's at the beginning of the study so use values from "preStudyStatus" 
  #IF IT WAS OUTSIDE THE STUDY AREA AND CAME BACK IN ITS FIRST DETECTION WILL BE THE DOWNSTREAM ARRAY
  #ie tag 989.001040499618
  #replace_na might be cleaner and faster but this is more descriptive
  mutate(`Study Area Status` = if_else(is.na(`Study Area Status`), preStudyStatus, `Study Area Status`)) %>%
  ungroup()

newProportionCounts <- allTagsStatusDfFilled %>%
  count(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) #%>%
#summarize(n = n())
#should add up to the total number of unique rows in release file bc based off newTag
totals <- proportionCounts %>%
  group_by(DetectionDate) %>%
  summarise(total = sum(n))
oldProportionCounts <- proportionCounts

y1 <- x3 %>%
  filter(preStudyStatus == "Outside study area", 
         StatusDate != as.Date("2025-11-11")) %>%
  distinct(newTag, .keep_all = TRUE)
# uniqueTags <- x3 %>%
#   distinct(newTag) %>%
#   count(newTag) %>%
#   mutate(nDigits = str_length(newTag))

#tag that 989.001040500063
x4 <- x3 %>%
  filter(is.na(`Study Area Status`))


#####filering troubelshooting
detectionData1 <- combinedDetectionAndStatusData$detectionsAttributesFlows
input <- list(
  slider2 = c(min(detectionData1$DetectionDate -1), max(detectionData1$DetectionDate +1)), 
  picker10 = unique(detectionData1$SPP), 
  arrayPicker = unique(detectionData1$antennaName),
  picker7 = unique(detectionData1$antenna), 
  slider10 = c(min(as.numeric(detectionData1$`TL 1st Enc. (mm)`), na.rm = TRUE), max(detectionData1$`TL 1st Enc. (mm)`, na.rm = TRUE)), 
  
)
dateColumnToFilter <- "StatusDate"
dailyStatus1 <- combinedDetectionAndStatusData$dailyStatus
detectionDatafiltered <- dailyStatus1  %>% 
  filter(
    .data[[dateColumnToFilter]] >= input$slider2[1] & .data[[dateColumnToFilter]] <= input$slider2[2],
    # antennaName %in% c(input$arrayPicker),
    # antenna %in% c(input$picker7),
    SPP %in% c(input$picker10),
    `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
    
  ) %>%
  arrange(detected)
####detection data 
dateColumnToFilter <- "DetectionDate"
alldetectionDatafiltered <- combinedDetectionAndStatusData$detectionsAttributesFlows  %>% 
  filter(
    .data[[dateColumnToFilter]] >= input$slider2[1] & .data[[dateColumnToFilter]] <= input$slider2[2],
    antennaName %in% c(input$arrayPicker),
    antenna %in% c(input$picker7),
    SPP %in% c(input$picker10),
    `TL 1st Enc. (mm)` >= input$slider10[1] & `TL 1st Enc. (mm)` <= input$slider10[2]
    
  ) %>%
  arrange(detected)

difs <- anti_join(dailyStatus1, `statusDataInAPp_2026-01-05`)
difsAlfilters <- anti_join(dailyStatus1, `statusData2_2026-01-05`)
difsstatusnew <- anti_join(dailyStatus1, `statusData3_2026-01-05`)

x <- dailyStatus1 %>%
  filter(is.na(`antenna`))

###trobeshooting a buit
dailyStatus1 <- combinedDetectionAndStatusData$dailyStatus
x1 <- dailyStatus1 %>%
  
  count(preStudyStatus)
x <- dailyStatus1 %>%
  count(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) 

x2 <- dailyStatus1 %>%
  distinct(.keep_all = TRUE) %>%
  filter(StatusDate == "2025-11-11" ) %>% #& StatusDate <= "2025-11-12" 
  count(newTag)

###########
detectionCountDataToDisplay <- combinedDetectionAndStatusData$dailyStatus %>%
  count(DetectionDate = StatusDate, `Antenna or Status` = `Study Area Status`) %>%
  group_by(DetectionDate) %>%
  mutate(Dailypercent = round(n / sum(n) * 100, 2))

detectionCountDataToDisplay$`Antenna or Status` <- 
  factor(detectionCountDataToDisplay$`Antenna or Status`,
         levels = unique(detectionCountDataToDisplay$`Antenna or Status`))

allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status` <- 
  factor(allDataFiltered()$detectionCountDataToDisplay$`Antenna or Status`,
         levels = statusOptions)
dateOptions <- unique(combinedDetectionAndStatusData$dailyStatus$`Release Date`)
x <- alldetectionDatafiltered %>%
  filter(`Release Date` %in% c(dateOptions))

x <- detectionCountDataToDisplay[,c("DetectionDate" =="2025-11-11")]
detectionCountDataToDisplay$n[detectionCountDataToDisplay$DetectionDate == "2025-11-11"
                              & detectionCountDataToDisplay$`Antenna or Status` == "Outside study area"]


#################
Paco_PIT_Tags_11_07_2023_Final_Zs_Unit <- read_excel("Paco_PIT Tags_11_07_2023 Final Zs Unit.xlsx")
x <- Paco_PIT_Tags_11_07_2023_Final_Zs_Unit %>%
  mutate(length = str_length(`DEC Tag ID`))

Unc_Tag_Releases_for_Sam_1_ <- read_excel("Unc Tag Releases for Sam (1).xlsx")
Unc_Tag_Releases_for_Sam_1_Counts <- Unc_Tag_Releases_for_Sam_1_ %>%
  rename(dec_tag = `Full Tag`) %>%
  distinct(dec_tag) %>%
  count(str_length(dec_tag))

Unc_Tag_Releases_for_Sam_1_csv <- read_csv("Unc Tag Releases for Sam (1) csv.csv", 
                                           col_types = cols(`Full Tag` = col_character()))

Unc_Tag_Releases_for_Sam_1_CSVCounts <- Unc_Tag_Releases_for_Sam_1_csv %>%
  rename(dec_tag = `Full Tag`) %>%
  distinct(dec_tag) %>%
  count(str_length(dec_tag))

detections <- read_excel("data/detections_20251216.xlsx", 
                         col_types = c("text", "text", "date", 
                                       "numeric", "text", "numeric"))

UncTagData <- read_csv("UncTagData.csv")
#CORRECT WAY
UncTagDataCharacterTag <- read_csv("UncTagData.csv", 
                       col_types = cols(dec_tag = col_character()))

UncTagDataCharacterTagcounts <- UncTagDataCharacterTag %>%
  distinct(dec_tag) %>%
  count(str_length(dec_tag))

detections_20251216 <- read_csv("detections_20251216.csv", 
                                col_types = cols(dec_tag = col_character()))
detections_20251216TagCounts <- detections_20251216 %>%
  distinct(dec_tag) %>%
  count(str_length(dec_tag))

csvUniqueTags <- UncTagData %>%
  distinct(dec_tag) %>%
  filter(str_length(dec_tag) == 15)
  count(str_length(dec_tag))
  write_csv(csvUniqueTags, "text.csv")

UncTagData1 <- UncTagData %>%
  filter(str_length(dec_tag) == 15) %>%
  distinct(dec_tag, .keep_all = TRUE)
UncTagDatax <- UncTagData %>%
  mutate(length = str_length(dec_tag)) %>%
  count(length)
#less digits
UncTagDataExcel <- read_excel("UncTagData.xlsx", 
                              col_types = c("text", "text", "date", 
                                            "numeric", "text", "numeric"))
UncTagDataExcel_x <- UncTagDataExcel %>%
  distinct(dec_tag) %>%
  mutate(length = str_length(dec_tag)) %>%
  count(length)
#18 digits
UncTagDataExcel2 <- read_excel("UncTagData more digits.xlsx", 
                              col_types = c("text", "text", "date", 
                                            "numeric", "text", "numeric"))
UncTagDataExcel2_1 <- UncTagDataExcel2 %>%
  #filter(str_length(dec_tag) == 18) %>%
  distinct(dec_tag, .keep_all = TRUE) %>%
  count(str_length(dec_tag))
  #mutate(last2 = str_sub(dec_tag, 17, 18))

countsLast2 <- UncTagDataExcel2_1 %>%
  count(last2)


UncTagDataExcel_x2 <- UncTagDataExcel2 %>%
  mutate(length = str_length(dec_tag)) %>%
  distinct(dec_tag, .keep_all = TRUE)
  count(length)

UncTagDataExcel_x <- UncTagDataExcel %>%
  mutate(length = str_length(dec_tag)) %>%
  count(length)

detectionsX <- detections %>%
  mutate(length = str_length(dec_tag)) %>%
  count(length)
# new release file qaqc
uncReleasesMARKFile_JanNew <- read_csv("data/Unc Tag Releases trout only jan new.csv")
x <- uncReleasesMARKFile_JanNew %>%
  count(`/*Tag#`)
