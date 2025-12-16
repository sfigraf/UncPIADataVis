library(shiny)
library(shinythemes)
library(tidyverse)
library(dataRetrieval) #for USGS 
library(leaflet) #for map
library(sf)
library(plotly)
library(shinydashboard) #for box()

antennaMetadata <- read_csv("data/antennaMetadata.csv")

antennasSF <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326)

neededFunctions <- c("getDailyand15MinUSGSData.R")

USGSFlows <- getDailyand15MinUSGSData("09147025", startDate = "2025-08-01", waterTemp = FALSE)

for (i in neededFunctions) {
  source(paste0("./functions/",i))
}

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

ui <- fluidPage(
  navbarPage(title = "Uncompahgre Data Exploration",
             id = "tabs", 
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it
             
             tabPanel("Map",
                      map_UI("map")
             ), 
             tabPanel("Discharge and Detections", 
                      environmentalData_UI("environmentalData")
                      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    map_Server("map", antennasSF)
    environmentalData_Server("environmentalData", USGSFlows$USGSDataDaily)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
