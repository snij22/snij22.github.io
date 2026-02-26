library(tidyverse)
library(osmdata)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")
options(tigris_use_cache = TRUE)
# set different Overpass server
set_overpass_url("https://overpass.private.coffee/api/interpreter")

franklin_city_limits <- places(state = "NH") |>
  filter(NAME == "Franklin") |>
  st_transform(crs = "EPSG:4326")

frank_bb <- st_bbox(franklin_city_limits)


road_type_code <- tribble(
  ~RTTYP , ~label             ,
  "C"    , "County"           ,
  "I"    , "Interstate"       ,
  "M"    , "Common Name"      ,
  "O"    , "Other"            ,
  "S"    , "State Recognized" ,
  "U"    , "U.S."
)

mer_roads <- roads("New Hampshire", "Merrimack")
frank_nad <- franklin_city_limits %>% st_transform(st_crs(mer_roads))

frank_roads <- st_crop(mer_roads, frank_nad) %>%
  left_join(road_type_code) %>%
  replace_na(list(label = "Other"))


tm_shape(frank_roads) +
  tm_lines(col = "label")


frank_roads %>%
  mutate(road_length = st_length(., )) %>%
  group_by(label) %>%
  summarise(
    total_road_length = sum(road_length),
    total_road_length = units::set_units(total_road_length, mi),
  )


frank_split <- frank_roads %>%
  split(.$label)


#this gets a bit more to responsibility: note that common name and other covers everything.
tm_shape(frank_roads) +
  tm_lines(
    col = "label",
    group = "All Roads",
    lwd = 4,
    group.control = "radio"
  ) +
  tm_shape(frank_split[['Common Name']]) +
  tm_lines(
    col = "label",
    group = "Common Name",
    col.legend = tm_legend(show = F),
    lwd = 4,
    group.control = "radio"
  ) +
  tm_shape(frank_split[['Other']]) +
  tm_lines(
    col = "label",
    group = "Other",
    col.legend = tm_legend(show = F),
    lwd = 4,
    group.control = "radio"
  ) +
  tm_shape(frank_split[['State Recognized']]) +
  tm_lines(
    col = "label",
    group = "State Recognized",
    col.legend = tm_legend(show = F),
    lwd = 4,
    group.control = "radio"
  ) +
  tm_shape(frank_split[['U.S.']]) +
  tm_lines(
    col = "label",
    group = "U.S.",
    col.legend = tm_legend(show = F),
    lwd = 4,
    group.control = "radio"
  ) +
  tm_basemap(leaflet::providers$OpenStreetMap, group.control = "check") +
  tm_title("Franklin Roads")
