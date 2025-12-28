#11/23/25

library(pdftools)
library(tidyverse)

fp<- list.files("data/prop_cards", full.names = T)


a<- pdftools::pdf_text(fp)

#good start but it struggles when multiple are on the same line
str_split(a, "\\n") %>% as_tibble(.name_repair= "unique") %>% 
  transmute(clean_name= str_squish(`...1`))  %>% filter(clean_name != "") %>% view()
  str_split(":")
  view()


b<- a %>% as_tibble(.name_repair= "unique") %>% 
  transmute(clean_name= str_squish(value))  %>% 
  str_split("^:")
  str_match(b, "NH Parcel ID: [:digit:]+-[:digit:]+-[:digit:]+")