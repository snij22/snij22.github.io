library(tidyverse)
library(osmdata)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")


# set different Overpass server
set_overpass_url("https://overpass.private.coffee/api/interpreter")
madison_city_limits <- places(state = "WI") |> filter(NAME == "Madison") |> st_transform(crs = "EPSG:4326")

franklin_city_limits <- places(state = "NH") |> filter(NAME == "Franklin") |> st_transform(crs = "EPSG:4326")

st_bbox(madison_city_limits)
st_bbox(franklin_city_limits)

bb <- c(-71.73366, 43.37770, -71.61998, 43.51793)
x <- opq (bbox = bb) |>
    add_osm_feature (key = "highway", value = c("primary", "secondary","tertiary", "motorway", "trunk", "unclassified", "residential")) |>
    osmdata_sf () 

tertiaries <- x$osm_lines


msn_tertiary <- tertiaries[franklin_city_limits, op = st_intersects]

msn_tertiary |> 
  mutate(maxspeed_numeric = as.numeric(str_remove(maxspeed, " mph")),
         maxspeed_factor = as_factor(maxspeed)) |> 
  st_drop_geometry() |> 
  count(maxspeed_numeric) |> 
  gt::gt()



msn_tertiary |> 
  mutate(maxspeed_numeric = as.numeric(str_remove(maxspeed, " mph")),
         thirty_mph = case_when(maxspeed_numeric < 30 ~ "below 30 mph",
                                maxspeed_numeric == 30 ~ "30 mph",
                                maxspeed_numeric > 30 ~ "over 30 mph"
                                 )) |> 
  tm_shape() +
  tm_lines(col = "highway",  lwd = 10)

msn_tertiary |> 
  mutate(maxspeed_numeric = as.numeric(str_remove(maxspeed, " mph")),
         thirty_mph = case_when(maxspeed_numeric < 30 ~ "below 30 mph",
                                maxspeed_numeric == 30 ~ "30 mph",
                                maxspeed_numeric > 30 ~ "over 30 mph"
                                 )) |> 
  tm_shape() +
  tm_lines(col = "surface",  lwd = 10)
### context https://rpubs.com/vgXhc/tertiary