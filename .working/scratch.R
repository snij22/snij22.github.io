library(tidyverse)
library(sf)
library(tmap)
library(gt)
library(DT)
tmap_mode("view")
prop_data <- read_csv(here::here(
  ".data/20251116_geo_dat/prop_card_data_combined.csv"
))
zone_sf <- read_sf(here::here(".data/20251116_geo_dat/zone_polys.shp"))

zone_type <- zone_sf %>%
  st_drop_geometry() %>%
  select(NAME, LABEL) %>%
  distinct() %>%
  mutate(lab2 = str_remove_all(LABEL, "-"))

better_zone_map <- read_sf(here::here(
  ".data/outputs/parcel_nums_with_zone.shp"
))

tax_rate <- 17.63 / 1000


dat <- prop_data %>%
  mutate(zone2 = ZONE, zone2 = str_remove_all(zone2, "(W&S)|(WORS)|(W/S)")) %>%
  left_join(zone_type, join_by(zone2 == lab2)) %>%
  distinct()

dat %>%
  group_by(NAME, LANDUSE) %>%
  filter(LABEL == "RS") %>%
  summarize(n = n())


sum_tab <- dat %>%
  select(NAME, TOTTXVAL) %>%
  mutate(NAME = str_remove_all(NAME, " District")) %>%
  group_by(NAME) %>%
  summarize(n_prop = n(), total_val = sum(TOTTXVAL, na.rm = T)) %>%
  mutate(
    tot_city = sum(total_val),
    tax = total_val * tax_rate,
    pct = round(total_val / tot_city, 4),
  ) %>%
  select(-tot_city, -total_val) %>%
  replace_na(list(NAME = "Missing Zoning"))

sum_gt <- sum_tab %>%
  summarize(across(where(is.numeric), sum)) %>%
  gt() |>
  cols_label(
    .list = c(
      n_prop = "Number of Properties",
      #total_val = "Total Tax Value",
      pct = "Percent of Total Tax Revenue",
      tax = "Estimated Tax"
    )
  ) %>%
  fmt_number(c(
    #total_val,
    tax
  )) %>%
  fmt_integer(n_prop) %>%
  fmt_percent(pct) %>%
  opt_interactive(use_pagination = FALSE)
sum_gt
