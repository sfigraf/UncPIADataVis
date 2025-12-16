##mapModule
#shinymod is snippet
map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  
  )
}

map_Server <- function(id, antennas) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet({
        centerCoords <- st_coordinates(st_centroid(st_union(antennas)))
        
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(maxZoom = 19.5), 
                           group = "Satellite"
          ) %>%
          setView(lng = centerCoords[1], lat = centerCoords[2], zoom = 19) %>%
          addAwesomeMarkers(
            data = antennas,
            group = "Detections",
            #clusterOptions = markerClusterOptions(),
            icon = icons(),
            label = paste(antennas$antenna, "\n"),
            #layerId = as.character(filtered_movements_data()$id),
            popup = paste(
              "Antenna Name:", antennas$antenna)
          )
      })
      
    }
  )
}