#11/22/25

library(tidyverse)
library(RSelenium)
library(rvest)
library(sf)

#pull all parcel polygons that I have
parcel_polys_fp<- here::here(".data/20251116_geo_dat/parcel_polys.shp")
parcel_nums<- st_read(dsn = parcel_polys_fp ) %>% st_drop_geometry() %>% 
  select(PARCEL_) %>%
  filter(PARCEL_ != "000-000-00")

read_a_tab_cell<- function(x){
  in_tab<- x %>% 
      filter(X3 != "") 
  p1<- in_tab %>% 
    select(1:2) %>%
    pivot_wider(names_from = X1, values_from = X2)

  in_tab<-in_tab %>%
    select(3:4) %>%
    pivot_wider(names_from = X3, values_from = X4)
  bind_cols(p1, in_tab)
}


#start server
rD<- RSelenium::rsDriver(browser = "firefox", phantomver = NULL, chromever = NULL,
 geckover = "0.36.0")
remDr <- rD[["client"]]

#go to this website
remDr$navigate("https://nhtaxkiosk.com/default.aspx")

franklin_hyper<- remDr$findElement(using = "id",
                                   value = "ctl00_ContentPlaceHolder1_HyperLink64")


franklin_hyper$clickElement()

raw_dat<- vector("list", nrow(parcel_nums))

parcel_nums<- parcel_nums %>%
  mutate(map_num = str_split(PARCEL_, "-", simplify = T)[,1], 
lot_num = str_split(PARCEL_, "-", simplify = T)[,2],
sub_num = str_split(PARCEL_, "-", simplify = T)[,3],)

existing<-  read_csv(here::here(".data/assess_ts_1.csv")) %>%
  select("Map-Lot-Sub") %>% unique() %>% filter( `Map-Lot-Sub` != "") %>%
  mutate(map_num = str_split_i( `Map-Lot-Sub`, "-", 1) %>% str_sub(., -3),
          lot_num = str_split_i( `Map-Lot-Sub`, "-", 2) %>% str_sub(., -3),
          sub_num = str_split_i( `Map-Lot-Sub`, "-", 3) %>% str_sub(., -2)) %>%
  transmute(PARCEL_ = paste(map_num, lot_num, sub_num, sep="-")) 

re_try<- anti_join(parcel_nums, existing)

################################################################################

for(i in 1:nrow(re_try)){
  # try catch
  skip_to_next<-FALSE


  parcel_full<- re_try[i,]
  
  map_num<- parcel_full$map_num
  lot_num<- parcel_full$lot_num
  sub_num<- parcel_full$sub_num
  

  map_box<- remDr$findElement(using = "id",
                                   value = "ctl00_ContentPlaceHolder1_txtPID1")

  lot_box<- remDr$findElement(using = "id",
                                   value = "ctl00_ContentPlaceHolder1_txtPID2")

  sub_box<- remDr$findElement(using = "id",
                                   value = "ctl00_ContentPlaceHolder1_txtPID3")

  map_box$clearElement()
  lot_box$clearElement()
  sub_box$clearElement()

  
  #send the parcel number to the serach box and hit enter
  map_box$sendKeysToElement(list(map_num))
  lot_box$sendKeysToElement(list(lot_num))
  sub_box$sendKeysToElement(list(sub_num))
  
  search_button_1<- remDr$findElement(using = "id", 
    value = "ctl00_ContentPlaceHolder1_lnkSearchPID")
  
  search_button_1$highlightElement()
  search_button_1$clickElement()

  Sys.sleep(3)
  # wrap in try catch
  parcel_hyper<- tryCatch(remDr$findElement(using = "id",   value = "ctl00_ContentPlaceHolder1_TaxBillsByPID_ctl02_hyperDetails"),  error = function(e) {skip_to_next <<- TRUE})
  
  
  if(skip_to_next) {next}


  parcel_hyper$clickElement()
  Sys.sleep(3)

############ on the results page#######

  page_results<- remDr$getPageSource()[[1]]
#  net_assessments<- read_html(page_results) %>% html_elements(css = "[id$=lblNetAssessment]", ) %>% html_text()
#  due_date<- read_html(page_results) %>% html_elements(css = "[id$=DueDate]") %>% html_text()

  full_tab<- read_html(page_results) %>% html_table()  %>% keep(~ncol(.x)==4)  %>% map(read_a_tab_cell)  %>%
  bind_rows() 



  raw_dat[[i]]<- full_tab

   
    Sys.sleep(.5)
  #scrape it and pull 
  print(i)
  remDr$goBack()
}







#save out a version
combined<- raw_dat %>% discard(is.null) %>% bind_rows() %>% select(1:12) 

data.table::fwrite(combined, here::here("data/assess_ts_2.csv"))

#if you forget to kill
#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
#linux killall -9 java
remDr$close()
rD$server$stop()
  