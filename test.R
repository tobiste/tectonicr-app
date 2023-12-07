library(leaflet)
library(dplyr)
library(tectonicr)
library(ggplot2)


# land <- rnaturalearth::ne_download(
#   #scale = 110,
#   type = "land",
#   category = "physical",
#   returnclass = "sf"
# ) |>
#   sf::st_geometry()
# saveRDS(land, "data/rnaturalearth_land.rds")
land = readRDS("data/rnaturalearth_land.rds")


data("san_andreas")
data("plates")

stress_df <- readRDS("data/wsm2016.rds") |>
  mutate(
    angle_map = tectonicr::deg2rad(90-azi),
    radius_map = scales::rescale(unc, from = c(0, 40), to = c(1, 0.2))
  )


# stress_colors_df <- data.frame(
#   color = stress_colors(),
#   regime = names(stress_colors())
# )
#
# stress_df <- stress_df |> left_join(
#   stress_colors_df
# )
#
# direction_marker <- makeAwesomeIcon(icon = "git-commit-sharp", library = "ion", markerColor = "white", squareMarker = TRUE,
#                                     iconRotate = stress_df$azi,
#                                     iconColor = stress_df$color
# )
#
#
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addPolylines(data = plates, color = "red", popup = plates$name) %>%
#   addAwesomeMarkers(lng=stress_df$lon, lat=stress_df$lat, popup = stress_df$id, icon = direction_marker, label =  as.character(stress_df$id))
# m  # Print the map
#
#


lat_lim = range(10, 50)
lon_lim = range(-120, -60)

stress_df_2_plot <- filter(
  stress_df,
  between(lat, lat_lim[1], lat_lim[2]),
  between(lon, lon_lim[1], lon_lim[2]),
)

# stress_df_2_plot <- stress_df |> sf::st_intersection()
#
#
# m <- ggplot() +
#   geom_sf(data = land) +
#   geom_sf(data = plates, color = "red") +
#   geom_spoke(data = stress_df_2_plot, aes(lon, lat, angle =angle_map, color = regime, radius = radius_map)) +
#   scale_color_manual("Stress regime", values =  stress_colors()) +
#   theme_bw() +
#   labs(x = "", y = "") +
#   coord_sf(xlim = range(stress_df_2_plot$lon), ylim = range(stress_df_2_plot$lat), expand = FALSE)
# m
#

library(ggiraph)


mi <- ggplot() +
  geom_sf(data = land) +
  geom_sf(data = plates, color = "red") +
  geom_spoke_interactive(data = stress_df_2_plot, aes(lon, lat, angle =angle_map, color = regime, radius = radius_map, tooltip = locality, data_id = id), hover_nearest = FALSE) +
  scale_color_manual("Stress regime", values =  stress_colors()) +
  theme_bw() +
  labs(x = "", y = "") +
  coord_sf(xlim = range(stress_df_2_plot$lon), ylim = range(stress_df_2_plot$lat), expand = FALSE)


girafe(ggobj = mi,
       options = list(
         opts_selection(
           type = "single",
           only_shiny = FALSE),
         opts_tooltip(use_fill = TRUE),
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "stroke-width:2;"),
         opts_zoom(max = 5)
       )
       )

