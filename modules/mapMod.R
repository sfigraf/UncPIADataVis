##mapModule
#shinymod is snippet
map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  
  )
}

map_Server <- function(id, antennasSF, detectionsSF) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet({
        centerCoords <- st_coordinates(st_centroid(st_union(antennasSF)))
        
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(maxZoom = 19.5), 
                           group = "Satellite"
          ) %>%
          setView(lng = centerCoords[1], lat = centerCoords[2], zoom = 19) %>%
          addAwesomeMarkers(
            data = antennasSF,
            group = "Antennas",
            #clusterOptions = markerClusterOptions(),
            icon = icons(),
            label = paste(antennasSF$antennaName, "\n"),
            #layerId = as.character(filtered_movements_data()$id),
            popup = paste(
              "Antenna Name:", antennasSF$antennaName)
          ) %>%
          addAwesomeMarkers(
            data = detectionsSF,
            group = "Detections",
            #clusterOptions = markerClusterOptions(),
            icon = icons(),
            label = paste(detectionsSF$antenna, "\n"),
            #layerId = as.character(filtered_movements_data()$id),
            popup = paste(
              "Antenna Name:", detectionsSF$antenna)
          ) %>%
          addLayersControl(overlayGroups = c("Detections", "Antennas"), 
                           baseGroups = c("Satellite")
          ) %>%
          hideGroup(c("Detections"))
      })
      
    }
  )
}