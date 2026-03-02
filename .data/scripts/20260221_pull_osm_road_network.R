library(tidyverse)
library(osmdata)
library(sf)
library(tigris)

#accessed on 3/2/26
# set different Overpass server

franklin_city_limits <- places(state = "NH") |>
  filter(NAME == "Franklin") |>
  st_transform(crs = "EPSG:4326")

#st_area(franklin_city_limits) %>% units::set_units(mi^2)

frank_bb <- st_bbox(franklin_city_limits)


x <- opq(bbox = frank_bb) |>
  add_osm_feature(
    key = "highway",
    value = c(
      "primary",
      "secondary",
      "tertiary",
      "motorway",
      "trunk",
      "unclassified",
      "residential"
    )
  ) |>
  osmdata_sf()

roads <- x$osm_lines
roads %>%
  janitor::clean_names() %>% #clean names for proper saving
  st_write(".data/outputs/20260221_osm_roads/20260221_frank_roads.shp")

st_write(
  franklin_city_limits,
  ".data/outputs/20260221_frank_limits/20260221_frank_city_limits.shp"
)
