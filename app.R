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
neededFunctions <- c("getDailyand15MinUSGSData.R")

for (i in neededFunctions) {
  source(paste0("./functions/",i))
}



USGSFlows <- getDailyand15MinUSGSData("09147025", startDate = min(date(detections$detected)), waterTemp = FALSE)

###Data Wrangling

Unc_Tag_Releases1 <- Unc_Tag_Releases %>%
  select(Date, SPP, `TL 1st Enc. (mm)`, `Full Tag`)

detections1 <- detections %>%
  mutate(newTag = if_else(str_length(dec_tag) == 18, substr(dec_tag, 1, nchar(dec_tag) - 2), dec_tag))

detectionsSF <- detections1 %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  left_join(Unc_Tag_Releases1, by = c("dec_tag" = "Full Tag")) %>%
  st_as_sf()

NAs <- detectionsSF %>%
  st_drop_geometry() %>%
  filter(is.na(SPP)) %>%
  distinct(newTag, .keep_all = TRUE)
#str_length("989.00103062026096")

dailyDetectionData <- detectionsSF %>%
  count(Date = date(detected), antennaName)
##########MOVEMENTS


# x <- detectionsSF %>%
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
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it
             tabPanel("Discharge and Detections", 
                      environmentalData_UI("environmentalData")
                      ), 
             tabPanel("Map",
                      map_UI("map")
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    environmentalData_Server("environmentalData", USGSFlows$USGSDataDaily, dailyDetectionData)
    map_Server("map", antennasSF, detectionsSF)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
