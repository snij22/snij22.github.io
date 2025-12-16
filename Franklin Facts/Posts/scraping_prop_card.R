#11/22/25

library(tidyverse)
library(RSelenium)
library(rvest)
library(sf)

#pull all parcel polygons that I have
parcel_polys_fp<- here::here("data/20251116_geo_dat/parcel_polys.shp")
parcel_nums<- st_read(dsn = parcel_polys_fp ) %>% st_drop_geometry() %>% 
  select(PARCEL_) %>%
  filter(PARCEL_ != "000-000-00")

#pull scraped data
existing_dat_fp<- list.files(here::here("data/20251116_geo_dat/"), "prop", full.names = T)

#read scraped data. creaate joiing col
existing <- existing_dat_fp %>%
 map(~data.table::fread(.x, colClasses = 'character', data.table = FALSE)) %>% bind_rows() %>% unique() %>%
  mutate(across(.cols = c(MAP, LOT),.fns =  ~str_sub(., -3)),
          SUB = str_sub(SUB, -2)) %>%
  mutate(PARCEL_ = paste(MAP, LOT, SUB, sep="-")) 

#check if there are any remaining parcels that I have that do not have data for
residual<- anti_join(parcel_nums, existing)

#start server
rD<- RSelenium::rsDriver(browser = "firefox", phantomver = NULL, chromever = NULL,
 geckover = "0.36.0")
remDr <- rD[["client"]]

#go to this website
remDr$navigate("https://www.axisgis.com/FranklinNH/#")


#zoom_button<- remDr$findElement(using = "id",
#                                   value = "navZoomInButton")

#zoom_button$clickElement()
#zoom_button$clickElement()
#zoom_button$clickElement()

#this is the search box to enter parcel numbers
search_box<- remDr$findElement(using = "id",
                                   value = "search-input")
raw_dat<- vector("list", nrow(parcel_nums))




for(i in 1:nrow(residual)){
  # try catch
  skip_to_next<-FALSE
  search_box$clearElement() 
  parcel<- residual[i,]
  #send the parcel number to the serach box and hit enter
  search_box$sendKeysToElement(list(parcel, key = "enter"))
  Sys.sleep(1.5)
  #create parcel search
 
   search_result<-  tryCatch(remDr$findElement(using = "id", paste("search-result",parcel, parcel, sep = "-" )),  error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next) {next}

  search_result$clickElement()

  Sys.sleep(1.5)
  #scrape it and pull 
  table_of_data<- remDr$findElement(using = "css selector", "div.table-content-to-show")
  tax_card_data<- table_of_data$getElementText()
  raw_dat[[i]]<- tax_card_data %>% str_split("\\n") 
  search_box$clearElement()

  print(parcel)
}


rectangle_dat<- function(x){
a<- x %>% unlist() %>% as_tibble() %>% 
  separate_wider_delim(cols = value, delim = ":", names_sep = "", too_few = "align_start") %>%
  pivot_wider(names_from = "value1", 
  values_from = "value2")
  a
}

#save out a version
combined<- raw_dat %>% discard(is.null) %>% map(rectangle_dat) %>% bind_rows()
data.table::fwrite(combined, here::here("data/20251116_geo_dat/prop_card_data_4.csv"))


#if you forget to kill
#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
remDr$close()
rD$server$stop()
  