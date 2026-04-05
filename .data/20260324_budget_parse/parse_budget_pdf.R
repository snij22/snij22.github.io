library(pdftools)
library(tidyverse)

fp <- list.files(".data/20260324_budget_parse/", "1.pdf", full.names = T)

a <- pdftools::pdf_ocr_data(fp)

a <- pdftools::pdf_ocr_text(fp, pages = c(6, 10))


#good start but it struggles when multiple are on the same line
str_split(a, "\\n") %>%
  as_tibble(.name_repair = "unique") %>%
  transmute(clean_name = str_squish(`...1`)) %>%
  filter(clean_name != "") %>%
  view()
str_split(":")
view()


b <- a %>%
  as_tibble(.name_repair = "unique") %>%
  transmute(clean_name = str_squish(value)) %>%
  str_split("^:")
str_match(b, "NH Parcel ID: [:digit:]+-[:digit:]+-[:digit:]+")
