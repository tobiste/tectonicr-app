wsm2016 <- read.csv("data/wsm2016_download.csv") |>
  rename_all(tolower) |>
  filter(azi != 999, depth <= 40, quality != "E") |>
  mutate(
    unc = ifelse(is.na(sd), quantise_wsm_quality(quality), sd),
    unc = ifelse(unc == 0, 1, unc),
    regime = ifelse(regime == "SS", "S", regime),
    regime = ifelse(regime == "TF", "T", regime),
    regime = ifelse(regime == "NF", "N", regime),
    regime = ifelse(regime == "U", NA, regime)
  ) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE)
saveRDS(wsm2016, "data/wsm2016.rds")

land <- rnaturalearth::ne_download(
  #scale = 110,
  type = "land",
  category = "physical",
  returnclass = "sf"
) |>
  sf::st_geometry()
saveRDS(land, "data/rnaturalearth_land.rds")

wgs84_bbox <- rnaturalearth::ne_download(
  type = "wgs84_bounding_box",
  category = "physical",
  returnclass = "sf"

)



nuvel_pb <- sf::read_sf("E:/Global data/plate_boundaries/NUVEL-1A_plate_boundaries.shp") |>
  #smoothr::densify() |>
  sf::st_difference(wgs84_bbox)
#nuvel_pb <- nuvel_pb[lengths(sf::st_intersects(nuvel_pb, wgs84_bbox)) == 0,]
#plot(nuvel_pb)






morvel_pb <- sf::read_sf("E:/Global data/plate_boundaries/MORVEL56_plates_outlines.shp") |>
  sf::st_wrap_dateline()

pb2002_pb <- sf::read_sf("E:/Global data/plate_boundaries/PB2002/PB2002_boundaries.shp") |>
  sf::st_wrap_dateline()


list(
  "nuvel" = nuvel_pb,
  "morvel" = morvel_pb,
  "pb2002" = pb2002_pb
) |>
  saveRDS("data/plate_boundaries.rds")
