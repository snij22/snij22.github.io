library(tidyverse)

existing_fp<- list.files(here::here("data/"), "assess", full.names = T)

existing<- existing_fp %>% map(data.table::fread) %>% bind_rows()

a<-  existing %>% mutate(across(c(`Bill Amount`, Principal, Interest, Penalties, `Total Due`), parse_number), 
                        date = mdy(`Due Date`))
  
a %>%filter(str_detect(`Current Owner`, "PARICHAND")) %>% 
  filter(str_detect(Type, "Property")) %>% 
  ggplot(aes(x = date, y = `Bill Amount`)) + 
  geom_path() + 
  geom_point()

b<- a %>% filter( `Map-Lot-Sub` != "") %>%
  mutate(map_num = str_split_i( `Map-Lot-Sub`, "-", 1) %>% str_sub(., -3),
          lot_num = str_split_i( `Map-Lot-Sub`, "-", 2) %>% str_sub(., -3),
          sub_num = str_split_i( `Map-Lot-Sub`, "-", 3) %>% str_sub(., -2), 
          PARCEL_ = paste(map_num, lot_num, sub_num, sep="-")) 
data.table::fwrite(b, "data/assess_ts_comb.csv")


card_fp<- list.files(here::here("data/20251116_geo_dat"), "combined.csv$", full.names = T)

card_info<- data.table::fread(card_fp)%>% distinct()
card_sm<- card_info %>% select(PARCEL_, ZONE, MODEL, LANDUSE, OWNER)  %>%
  select(PARCEL_, ZONE)%>% distinct()
combo_data<- b %>%
  left_join(card_sm)
combo_data %>% filter(ZONE == "R2") %>% 
  ggplot(aes(x = date, y = `Bill Amount`, group = PARCEL_)) + 
  geom_point() + 
  geom_line(alpha = .1) 
