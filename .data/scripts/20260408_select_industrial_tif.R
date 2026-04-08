library(tidyverse)
library(sf)
library(gt)
library(tmap)
tmap_mode("view")


better_zone_map <- read_sf(here::here(
  ".data/outputs/parcel_nums_with_zone.shp"
))

ind_tif_parc <- c(
  "082-009-02",
  "082-408-01",
  "082-408-02",
  "082-409-00",
  "083-012-00",
  "084-401-00",
  "100-030-00",
  "100-403-00",
  "100-410-00",
  "101-001-00",
  "101-002-00",
  "101-003-00",
  "101-006-00",
  "101-008-00",
  "101-009-00",
  "101-009-01",
  "101-009-02",
  "101-009-03",
  "101-009-04",
  "101-009-05",
  "101-009-06",
  "101-009-07",
  "101-009-08",
  "101-009-09",
  "101-009-10",
  "101-009-11",
  "101-009-12",
  "101-010-00",
  "101-401-00",
  "101-402-00",
  "101-403-00",
  "101-404-00",
  "102-004-00",
  "102-009-00",
  "102-010-00",
  "102-011-00",
  "102-402-00",
  "102-403-00",
  "102-403-01",
  "102-403-02",
  "102-403-03",
  "103-005-00",
  "103-006-00",
  "103-405-00",
  "103-406-00",
  "082-408-03",
  "082-408-00"
)

better_zone_map %>%
  filter(PARCEL_ %in% ind_tif_parc) %>%
  st_cast("MULTIPOLYGON") %>%
  summarise("industrial_overlay") %>%
  st_concave_hull(0.25) %>%
  st_write(".data/20260408_industrial_tif/industrial_tif_overlay.shp")
better_zone_map %>%
  filter(PARCEL_ %in% ind_tif_parc) %>%
  st_write(".data/20260408_industrial_tif/industrial_tif_parc.shp")
