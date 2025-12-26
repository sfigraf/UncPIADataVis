library(shiny)
library(shinythemes)
library(tidyverse)
library(dataRetrieval) #for USGS 
library(leaflet) #for map
library(sf)
library(plotly)
library(shinydashboard) #for box()
library(readxl)
library(shinycssloaders) #withSpinner
library(DT)
library(shinyWidgets) # for pickerInputs

antennaMetadata <- read_excel("data/antennaMetadata.xlsx")
#detections_20251216 <- read_csv("data/detections_20251216.csv")
detections <- read_excel("data/detections_20251216.xlsx", 
                         col_types = c("text", "text", "date", 
              "numeric", "text", "numeric"))
Unc_Tag_Releases <- read_excel("data/Unc Tag Releases.xlsx", 
                               col_types = c("date", "text", "numeric", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric"))

antennasSFAll <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326) 
antennasSF <- antennasSFAll %>%
  distinct(geometry, .keep_all = TRUE)


for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}
neededFunctions <- c("getDailyand15MinUSGSData.R", "getMovementsFunction.R")

for (i in neededFunctions) {
  source(paste0("./functions/",i))
}



USGSFlows <- getDailyand15MinUSGSData("09147025", startDate = min(date(detections$detected)), waterTemp = FALSE)

###Data Wrangling

Unc_Tag_Releases1 <- Unc_Tag_Releases %>%
  select(`Release Date` = Date, SPP, `TL 1st Enc. (mm)`, `Full Tag`)

detections1 <- detections %>%
  mutate(newTag = if_else(str_length(dec_tag) == 18, substr(dec_tag, 1, nchar(dec_tag) - 2), dec_tag))
detectionsAttributesFlows <- detections1 %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  left_join(Unc_Tag_Releases1, by = c("newTag" = "Full Tag")) %>%
  mutate(DetectionDate = date(detected)) %>%
  left_join(USGSFlows$USGSDataDaily, by = c("DetectionDate" = "Date"))
  #st_as_sf()

#####QAQC
morethan1dec_tag <- detectionsAttributesFlows %>%
  distinct(dec_tag, newTag) %>%
  count(`Release File tag entry` = newTag, name = "Number of dec_tag Entries") %>%
  filter(`Number of dec_tag Entries` > 1)

# NARaw <- detectionsAttributesFlows %>%
#   st_drop_geometry() %>%
#   filter(is.na(SPP)) 
# 
# NACounts <- NARaw %>%
#   count(newTag)
# 
# NAs <- NARaw %>%
#   distinct(newTag, .keep_all = TRUE)
#str_length("989.00103062026096")

# dailyDetectionData <- detectionsAttributesFlows %>%
#   count(Date = date(detected), antennaName)
##########MOVEMENTS


# x <- detectionsAttributesFlows %>%
#   group_by(dec_tag) %>%
#   arrange(detected) %>%
#   #filter(dec_tag == "3DD.0078E38638") %>%
#   mutate(movement = case_when(str_detect(antennaName, c("Downstream")) & str_detect(lag(antennaName), c("Upstream")) ~ "Downstream Movement", 
#                               str_detect(antennaName, c("Upstream")) & str_detect(lag(antennaName), c("Downstream")) ~ "Upstream Movement", 
#                               antennaName == "Cow Creek Antenna" ~ "Cow Creek Detection",
#                               antennaName == lag(antennaName) ~ "No Movement",
#                               TRUE ~ NA
#                               )
#                               )
#   

ui <- fluidPage(
  navbarPage(title = "Uncompahgre Data Exploration",
             id = "tabs", 
             theme = shinytheme("journal"), #end of navbar page arguments; what follow is all inside it
             tabPanel("Discharge and Detections", 
                      environmentalData_UI("environmentalData", detectionsAttributesFlows)
                      ), 
             tabPanel("Map",
                      map_UI("map")
             ), 
             tabPanel("QAQC", 
                      QAQC_UI("qaqc")
                      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    environmentalData_Server("environmentalData", USGSFlows$USGSDataDaily, detectionsAttributesFlows)
    map_Server("map", antennasSF, detectionsAttributesFlows)
    QAQC_Server("qaqc", morethan1dec_tag)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
