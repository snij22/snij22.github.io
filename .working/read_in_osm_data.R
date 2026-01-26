library(osmdata)
library(sf)
library(tidyverse)
library(tmap)
a<- getbb("Franklin, NH", format_out = "polygon") %>% st_as_sf()
qtm(a)
a<- opq("Franklin, NH") %>% 
  add_osm_feature(key = "highway", value = "road")
tt<- osmdata_sf(a)
zone_fp<- here::here(".data/20251116_geo_dat/zone_polys.shp")
zone_polys<- st_read(zone_fp, quiet = T)
borders<- zone_polys %>% summarise() %>% st_concave_hull(ratio = .17)

bb <- getbb ("franklin nh USA")
query <- opq (bb) |>
    add_osm_feature (key = "highway")
# Then extract data from 'Overpass' API
dat <- osmdata_sf (query, quiet = FALSE)
