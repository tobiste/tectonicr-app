library(shiny)
library(dplyr)
library(tectonicr)
library(sf)
library(ggplot2)
library(ggiraph)
# library(gt)


stress_df <- readRDS("data/wsm2016.rds") |>
  dplyr::mutate(
    angle_map = tectonicr::deg2rad(90 - azi),
    radius_map = scales::rescale(unc, from = c(0, 40), to = c(1, 0.2))
  )

land <- readRDS("data/rnaturalearth_land.rds")
plate_boundaries <- readRDS("data/plate_boundaries.rds")
data("cpm_models", package = "tectonicr")
