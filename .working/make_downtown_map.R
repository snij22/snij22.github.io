library(tidyverse)
library(osmdata)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")
# set different Overpass server
set_overpass_url("https://overpass.private.coffee/api/interpreter")

franklin_city_limits <- places(state = "NH") |>
  filter(NAME == "Franklin") |>
  st_transform(crs = "EPSG:4326")

frank_bb <- st_bbox(franklin_city_limits)

ttm()
a <- tm_shape(franklin_city_limits) +
  tm_borders()
tm_basemap(leaflet::providers$OpenStreetMap)


ggplot(franklin_city_limits) +
  geom_sf()


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

tertiaries <- x$osm_lines


msn_tertiary <- tertiaries[franklin_city_limits, op = st_intersects]
