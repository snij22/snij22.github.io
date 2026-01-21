library(sf)
library(tidyverse)
library(units)
library(tmap)

parcel_path<- here::here(".data/20251116_geo_dat/")

parcels<- read_sf(paste0(parcel_path, "parcel_polys.shp"))

zones<- read_sf(paste0(parcel_path, "zone_polys.shp")) %>% st_zm() %>%
  st_transform(st_crs(parcels))
parc<- parcels %>% select(PARCEL_)%>% group_by(PARCEL_) %>% st_cast("MULTIPOLYGON")

zone<- zones %>% group_by(NAME) %>% st_cast("MULTIPOLYGON")


joined_data<-st_join(parc, zone, largest = T)
dat<- joined_data %>% 
  mutate(area =st_area(.)) %>% filter(PARCEL_ != "000-000-00")

a<- dat %>% st_drop_geometry() %>% group_by(NAME) %>%
  summarize(n = n(), 
            tot_area = sum(area), 
            mean_area = mean(area))


b<- zone %>% ungroup ()%>% mutate(area = st_area(.)) %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarize(n = n(), 
            tot_area = sum(area), 
            mean_area = mean(area))

left_join(a, b, join_by(NAME
))  %>% select(NAME, starts_with("tot")) %>%
  mutate(difference = tot_area.x - tot_area.y, 
         diff = set_units(difference, acre))
  

ttm()
qtm(zones)

tm_shape(parc) + 
  tm_polygons() + 
  tm_shape(zone) + 
  tm_polygons("NAME", fill_alpha = .3, tm_scale_categorical(values.repeat = F)) 

tm_shape(joined_data) + 
  tm_polygons("NAME", tm_scale_categorical(values.repeat = F))


joined_data %>% select(PARCEL_, NAME, LABEL, TYPE) %>% 
write_sf( ".data/outputs/parcel_nums_with_zone.shp")
                       