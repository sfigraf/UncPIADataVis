library(shiny)
library(shinythemes)
library(tidyverse)
library(dataRetrieval) #for USGS 
library(leaflet) #for map
library(sf)

antennaMetadata <- read_csv("data/antennaMetadata.csv")

antennasSF <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326)

neededFunctions <- c("getDailyand15MinUSGSData.R")

for (i in neededFunctions) {
  source(paste0("./functions/",i))
}

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(title = "Uncompahgre Data Exploration",
             id = "tabs", 
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it
             
             tabPanel("Map",
                      map_UI("map")
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    map_Server("map", antennasSF)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
