library(tidyverse)

tax_rate_table <- tribble(
  ~year , ~tax_rate ,
   2014 , 24.95     ,
   2015 , 25.03     ,
   2016 , 25.23     ,
   2017 , 25.56     ,
   2018 , 21.96     ,
   2019 , 22.47     ,
   2020 , 22.84     ,
   2021 , 23.21     ,
   2022 , 24.39     ,
   2023 , 16.26     ,
   2024 , 17.15     ,
   2025 , 17.63
)


ind_tif <- tibble(
  year = 2024,
  tif_tax = 62290,
  value = tif_tax / (17.15 / 1000)
)
tif_value <- ind_tif$value
tif_tbl <- tax_rate_table %>%
  mutate(tif_value, tif_tax = (tif_value / 1000) * tax_rate)


tif_val <- tribble(
  ~year , ~tif_overlay ,
   2026 ,        48378 ,
   2025 ,        55525 ,
   2024 ,        62290 ,
   2023 ,        21587 ,
   2022 ,        21243 ,
   2021 ,        23307 , #0?
   2020 ,       101449 , #0?
   2019 ,        40786 ,
   2018 ,        40786 , #
   2017 ,        16482 , #
   2016 ,        14582
) #

tif_val %>%
  left_join(tax_rate_table)

tif_val %>%
  ggplot(aes(x = year, y = tif_overlay)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 70000)
