library(shiny)
library(shinythemes)
library(tidyverse)
library(dataRetrieval) #for USGS 
library(leaflet) #for map
library(sf)
library(plotly)
library(shinydashboard) #for box()
library(readxl)

antennaMetadata <- read_excel("data/antennaMetadata.xlsx")
detections <- read_csv("data/detections_20251216.csv")

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

detectionsSF <- detections %>%
  left_join(antennasSFAll, by = c("antenna" = "antennaNumber"
  )) %>%
  st_as_sf()


dailyDetectionData <- detectionsSF %>%
  count(Date = date(detected), antennaName)
  

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
