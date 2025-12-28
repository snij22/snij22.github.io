#11/23/25


library(tidyverse)
library(sf)
library(units)

#pull all parcel polygons that I have
parcel_polys_fp<- here::here("data/20251116_geo_dat/parcel_polys.shp") 
parcel_polys<- st_read(parcel_polys_fp)

card_fp<- list.files(here::here("data/20251116_geo_dat"), "combined.csv$", full.names = T)

card_info<- data.table::fread(card_fp)%>% distinct()
card_sm<- card_info %>% select(PARCEL_, ZONE, MODEL, LANDUSE, OWNER) %>% distinct()

#pj = parcel joined for smultipolygons
pj<- parcel_polys%>%
  group_by(PARCEL_) %>% 
  summarise()

join_dat<- pj %>% left_join(card_sm)


zone_plot<-ggplot(join_dat) + 
  geom_sf(aes(fill =ZONE )) + 
  theme_void()


use_plot<- ggplot(join_dat) + 
  geom_sf(aes(fill =LANDUSE))+ 
  theme_void()


model_plot<- ggplot(join_dat) + 
  geom_sf(aes(fill =MODEL))+ 
  theme_void()



#i'm pretty sure this is all zoned as water
na_zone<- join_dat %>% 
  filter(is.na(ZONE )) 
ggplot(na_zone)+ 
  geom_sf(aes(fill = ZONE))


sum_prep<- join_dat %>%
  mutate(area_acre = st_area(.) %>% set_units("acres")) %>%
  filter(!is.na(ZONE)) %>%
  mutate(total_area_in_franklin = sum(area_acre),
pct_area = (area_acre/total_area_in_franklin) *100)


zone_sum<-sum_prep %>% group_by(ZONE) %>%
  summarise(total_area_acre=sum(area_acre) %>% round(2), 
            percentage_area= sum(pct_area) %>% round(2)) %>% st_drop_geometry()

landuse_sum<-sum_prep %>% group_by(LANDUSE) %>%
  summarise(total_area_acre=sum(area_acre) %>% round(2), 
            percentage_area= sum(pct_area) %>% round(2)) %>% st_drop_geometry()

model_sum<-sum_prep %>% group_by(MODEL) %>%
  summarise(total_area_acre=sum(area_acre) %>% round(2), 
            percentage_area= sum(pct_area) %>% round(2)) %>% st_drop_geometry()


out<- list()
out[["zoning"]]<- zone_sum

out[["landuse"]]<- landuse_sum
out[["model"]]<- model_sum
out[["raw"]]<- sum_prep%>% st_drop_geometry()

writexl::write_xlsx(out, "data/outputs/20251123_summarized_zon_type.xlsx")


aa<- c(zone_plot, use_plot, model_plot)

ttm()
interactive_map<- tm_shape(join_dat, name = "ZONE")+
  tm_polygons(fill="ZONE",) + 
  tm_shape(join_dat, name = "Land Use") + 
  tm_polygons("LANDUSE") + 
  tm_shape(join_dat, name = "Model") + 
  tm_polygons("MODEL")





tmap::tmap_save(interactive_map, "data/outputs/20251123_interactive_map.html")

?pdf("data/outputs/20251123_maps.pdf", width= 8, height = 11)
aa
dev.off()
library(tmap)
