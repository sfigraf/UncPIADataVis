#notes
library(dataRetrieval)

antennas <- st_as_sf(antennaMetadata, coords = c("long", "lat"), crs = 4326)
centerCoords <- st_coordinates(st_centroid(st_union(antennas)))
fitBounds(antennaMetadata$long[1], antennaMetadata$lat[1], antennaMetadata$long[nrow(antennaMetadata)], 
          antennaMetadata$lat[nrow(antennaMetadata)])
