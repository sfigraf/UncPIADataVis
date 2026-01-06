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

combinedDetectionAndStatusData <- readRDS("data/flatFilesforApp/combinedDetectionAndStatusData.rds")
USGSFlows <- readRDS("data/flatFilesforApp/USGSFlows.rds")
antennasSF <- readRDS("data/flatFilesforApp/antennasSF.rds")
qaqcData <- readRDS("data/flatFilesforApp/qaqcData.rds")

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

##Color assignment
USGSLineColor <- setNames("#87CEEB", "USGSLineColor")
antennaNameOptions <- sort(unique(combinedDetectionAndStatusData$detectionsAttributesFlows$antennaName))
statusOptions <- sort(unique(combinedDetectionAndStatusData$dailyStatus$`Study Area Status`))

antennaNameColorsOptions <- c("#A67b5b", "#F8696B", "#63BE7B")
antennaNameColors <- setNames(antennaNameColorsOptions, antennaNameOptions)

statusColorsOptions <- c("#A67b5b", "#63BE7B", "#F8696B")
statusColors <- setNames(statusColorsOptions, statusOptions)

allColors <- c(statusColors, antennaNameColors, USGSLineColor)

ui <- fluidPage(
  navbarPage(title = "Uncompahgre Data Exploration",
             id = "tabs", 
             theme = shinytheme("journal"), #end of navbar page arguments; what follow is all inside it
             tabPanel("Discharge and Detections", 
                      environmentalData_UI("environmentalData", combinedDetectionAndStatusData)
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
    environmentalData_Server("environmentalData", USGSFlows$USGSDataDaily, combinedDetectionAndStatusData, allColors)
    map_Server("map", antennasSF)
    QAQC_Server("qaqc", qaqcData)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
