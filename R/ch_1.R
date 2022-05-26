#### Analysis for chapter 1 ####################################################

library(tidyverse)
library(lubridate)
qs::qload("output/data/ch_1.qsm")



# Active listings and revenue ---------------------------------------------

active_all_avg_2021 <- 
  daily |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

active_non_housing_2021 <- 
  daily |> 
  filter(!housing, year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

active_avg_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

hosts_avg_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

rev_total_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021 <- scales::dollar(rev_host_2021$avg, 100)
rev_med_2021 <- scales::dollar(rev_host_2021$med, 100)

active_avg_2019 <- 
  daily |> 
  filter(housing, year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

active_change_pct <- 
  ((parse_number(active_avg_2019) - parse_number(active_avg_2021)) / 
  parse_number(active_avg_2019)) |> 
  scales::percent(0.1)

hosts_avg_2019 <- 
  daily |> 
  filter(housing, year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

hosts_change_pct <- 
  ((parse_number(hosts_avg_2019) - parse_number(hosts_avg_2021)) / 
     parse_number(hosts_avg_2019)) |> 
  scales::percent(0.1)

rev_total_2019 <- 
  daily |> 
  filter(housing, year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_change_pct <- 
  ((parse_number(rev_total_2019) - parse_number(rev_total_2021)) / 
     parse_number(rev_total_2019)) |> 
  scales::percent(0.1)

rev_host_2019 <- 
  daily |> 
  filter(housing, year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019 <- scales::dollar(rev_host_2019$avg, 100)
rev_med_2019 <- scales::dollar(rev_host_2019$med, 100)

rev_avg_change_pct <- ((rev_host_2019$avg - rev_host_2021$avg) / 
  rev_host_2019$avg) |> 
  scales::percent(0.1)

rev_med_change_pct <- ((rev_host_2019$med - rev_host_2021$med) / 
  rev_host_2019$med) |> 
  scales::percent(0.1)

active_daily_tiers <- 
  daily |> 
  filter(housing, status %in% c("A", "R")) |> 
  left_join(select(property, property_ID, tier)) |> 
  count(date, tier)

active_daily_tiers <- 
  daily |> 
  filter(housing, status %in% c("A", "R")) |> 
  count(date) |> 
  mutate(tier = "All") |> 
  bind_rows(active_daily_tiers)
  


# Home sharers and commercial operators -----------------------------------

eh_pct_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status %in% c("A", "R")) |> 
  count(listing_type) |> 
  summarize(pct = n[listing_type == "Entire home/apt"] / sum(n)) |> 
  pull(pct) |> 
  scales::percent(0.1)

eh_pct_2019 <- 
  daily |> 
  filter(housing, year(date) == 2019, status %in% c("A", "R")) |> 
  count(listing_type) |> 
  summarize(pct = n[listing_type == "Entire home/apt"] / sum(n)) |> 
  pull(pct) |> 
  scales::percent(0.1)


# Save output -------------------------------------------------------------

qs::qsavem(active_all_avg_2021, active_non_housing_2021, active_avg_2021,
           hosts_avg_2021, rev_total_2021, rev_avg_2021, rev_med_2021,
           active_avg_2019, active_change_pct, hosts_avg_2019, hosts_change_pct,
           rev_total_2019, rev_change_pct, rev_avg_2019, rev_med_2019,
           rev_avg_change_pct, rev_med_change_pct, active_daily_tiers,
           eh_pct_2021, eh_pct_2019,
           file = "output/data/ch_1.qsm", nthreads = future::availableCores())
