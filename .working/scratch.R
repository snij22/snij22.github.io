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


tr_2025 <- 17.63
tax_rate <- tibble(increase = c(seq(.00, .1, .01)))
home_value <- tibble(hv = seq(200e3, 600e3, 50e3))

comb <- crossing(tax_rate, home_value) %>%
  #mutate(tax = ((hv / 1000) * (tr_2025 * (1 + increase)))) %>%
  mutate(tax = ((hv / 1000) * (tr_2025 + increase * 100))) %>%
  group_by(hv) %>%
  mutate(change = tax - lag(tax)) %>%
  replace_na(list(change = 0)) %>%
  mutate(change2 = cumsum(change))

comb %>%
  mutate(
    pct_labs = paste0(increase * 100, "%"),
    hv_label = scales::dollar(hv)
  ) %>%
  ggplot(aes(x = increase, y = tax, color = hv_label)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(n.breaks = 10, labels = scales::label_currency()) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_color_discrete("Home Value") +
  theme_bw() +
  labs(x = "Percentage Tax increase", y = "Estimated Tax Owed")


comb %>%
  mutate(
    pct_labs = paste0(increase * 100, "%"),
    hv_label = scales::dollar(hv)
  ) %>%
  ggplot(aes(x = fct_reorder(pct_labs, increase), y = change2)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::label_currency()) +
  facet_wrap(~hv_label) +
  labs(x = "Percentage Tax Increase", y = "Additional Tax Owed") +
  theme_bw()


###

library(tidyverse)
library(sf)
library(gt)
library(tmap)
tmap_mode("view")
better_zone_map <- read_sf(here::here(
  ".data/outputs/parcel_nums_with_zone.shp"
))

prop_data <- read_csv(here::here(
  ".data/20251116_geo_dat/prop_card_data_combined.csv"
)) %>%
  select(PARCEL_, LANDUSE) %>%
  distinct()


zone_sf <- read_sf(here::here(".data/20251116_geo_dat/zone_polys.shp"))

a <- zone_sf %>%
  st_zm() %>%
  st_transform(st_crs(better_zone_map)) %>%
  group_by(NAME) %>%
  filter(LABEL == "DROD" | LABEL == "LP")
peabody <- better_zone_map %>% filter(PARCEL_ == "117-138-00")
lakes_and_hist <- st_intersection(better_zone_map, a) %>%
  #bind_rows(peabody) %>%
  filter(
    !PARCEL_ %in%
      c(
        "000-WATER-00",
        "000-000-00",
        "117-405-00", #Odell
        "117-346-00",
        "117-321-00",
        "117-347-00",
        "117-320-00" #Downtown slivers
      )
  ) %>%
  mutate(lead_3 = str_sub(PARCEL_, start = 1, end = 3)) %>%
  mutate(group = if_else(lead_3 < 100, "Lake", "Downtown")) %>%
  group_by(PARCEL_) %>%
  slice(1) %>%
  left_join(prop_data) %>%
  replace_na(list(LANDUSE = "Missing")) # two missing properties of interest. One over the old penstock downtown and one right of way off webster av

ts_raw <- read_csv(here::here(".data/assess_ts_comb.csv"))

ts_dat <- ts_raw %>%
  filter(Type == "Property Tax") %>%
  select(PARCEL_, `Bill Amount`, `Due Date`) %>%
  janitor::clean_names() %>%
  mutate(due_date = mdy(due_date)) %>%
  filter(due_date >= "2015-01-01")

#median annual tax is 5033

lakes_and_hist %>%
  tm_shape() +
  tm_polygons(fill = "group")

comb <- lakes_and_hist %>%
  mutate(area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(PARCEL_, NAME, LABEL, area, group, LANDUSE) %>%
  distinct() %>%
  left_join(ts_dat, join_by(PARCEL_ == parcel))

tot_areas <- comb %>%
  group_by(group) %>%
  summarise(tot_area = sum(area), tot_area = units::set_units(tot_area, acre))

annual <- comb %>%
  mutate(year = year(due_date)) %>%
  drop_na(year) %>% #removes 3 properties. one with taxes at some point in the last 2 years (075-048-01)
  group_by(group, PARCEL_, year) %>%
  summarize(annual_tax = sum(bill_amount)) %>%
  filter(!(PARCEL_ == "075-036-00" & annual_tax > 50000)) #remove one obscenely high water bill (75k) from that parcel


growth <- annual %>%
  filter(year %in% c(2015, 2025)) %>%
  pivot_wider(names_from = year, values_from = annual_tax) %>%
  mutate(growth = `2025` - `2015`)

ggplot(growth, aes(x = group, y = growth / 1000)) +
  geom_boxplot() +
  geom_jitter(width = .1, height = 0, alpha = .3)

growth %>%
  left_join(lakes_and_hist) %>%
  st_as_sf() %>%
  select(PARCEL_, growth) %>%
  tm_shape() +
  tm_polygons(fill = "growth", fill.scale = tm_scale_continuous_pseudo_log())


ts_dat %>%
  mutate(year = year(due_date)) %>%
  drop_na(year) %>% #removes 3 properties. one with taxes at some point in the last 2 years (075-048-01)
  group_by(parcel, year) %>%
  summarize(annual_tax = sum(bill_amount)) %>%
  filter(annual_tax > 100) %>%
  mutate(log_tax = log(annual_tax, base = 10)) %>%
  ggplot(aes(x = as.factor(year), y = annual_tax)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::label_currency())


ts_dat %>%
  mutate(year = year(due_date)) %>%
  drop_na(year) %>% #removes 3 properties. one with taxes at some point in the last 2 years (075-048-01)
  group_by(parcel, year) %>%
  summarize(annual_tax = sum(bill_amount)) %>%
  filter(annual_tax > 100) %>%
  mutate(log_tax = log(annual_tax, base = 10)) %>%
  group_by(year) %>%
  summarize(med = median(annual_tax), quart = quantile(annual_tax)) %>%
  view()

#########################

library(tidyverse)
library(tidycensus)
library(tmap)
library(sf)
frank <- st_read(
  ".data/outputs/20260221_frank_limits/20260221_frank_city_limits.shp"
) %>%
  st_transform(st_crs(dat))

dat <- get_acs(
  geography = "block group",
  state = "NH",
  variables = "B01003_001",
  year = 2017,
  geometry = T
)

dat %>% st_unio(frank, ) %>% tm_shape() + tm_polygons("estimate")


load_variables(2017, "acs5", cache = TRUE)
