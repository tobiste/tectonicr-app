library(leaflet)
library(dplyr)
library(tectonicr)

data("san_andreas")
data("plates")




stress_colors_df <- data.frame(
  color = stress_colors(),
  regime = names(stress_colors())
)

san_andreas2 <- san_andreas |> left_join(
  stress_colors_df
)

direction_marker <- makeAwesomeIcon(icon = "arrow-up", markerColor = "white", squareMarker = TRUE,
                                    iconRotate = san_andreas$azi,
                                    iconColor = san_andreas2$color
)


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolylines(data = plates, color = "red") %>%
  addAwesomeMarkers(lng=san_andreas$lon, lat=san_andreas$lat, popup = san_andreas$id, icon = direction_marker )
m  # Print the map
