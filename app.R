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
    environmentalData_Server("environmentalData", USGSFlows$USGSDataDaily, combinedDetectionAndStatusData)
    map_Server("map", antennasSF)
    QAQC_Server("qaqc", qaqcData)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
