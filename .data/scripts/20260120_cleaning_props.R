library(tidyverse)
library(sf)
prop_data<- read_csv(".data/outputs/prop_card_data_combined_clean.csv") %>% distinct() 

zone_data<- read_sf(".data/outputs/parcel_nums_with_zone.shp") %>% 
  st_drop_geometry() %>% filter(TYPE == "ZONE") %>% distinct() 
comb<- prop_data %>% left_join(zone_data, relationship = 'many-to-many')  %>%  drop_na(ID) %>%  distinct() %>% group_by(PARCEL_) %>% slice(1)
 
  
mults<- comb %>% group_by(PARCEL_, ZONE) %>%
  summarize(numa = n()) %>% arrange("desc") %>% filter(numa>1)

comb %>% filter(PARCEL_ %in% mults$PARCEL_) %>% drop_na(ID) %>%  distinct() %>% group_by(PARCEL_) %>% slice(1)%>% view()

data.table::fwrite(comb, ".data/outputs/prop_data_with_zone.csv")
