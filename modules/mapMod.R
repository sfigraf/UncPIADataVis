##mapModule
#shinymod is snippet
map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  
  )
}

map_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(maxZoom = 19.5), 
                           group = "Satellite"
          )
      })
      
    }
  )
}