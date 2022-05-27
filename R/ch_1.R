#### Analysis for chapter 1 ####################################################

library(tidyverse)
library(lubridate)
qs::qload("output/data/data_processed.qsm", nthreads = future::availableCores())


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

revenue_colour <- colorRampPalette(c("#FF6600", "#FFCC66", "#CC6699", "#3399CC", 
                                     "#074387"))(10)

daily_for_rev <- 
  daily |>  
  filter(housing, status == "R", !is.na(host_ID), year(date) == 2021) |> 
  group_by(tier, host_ID) |> 
  summarize(rev = sum(price))

daily_for_rev <- 
  daily |>  
  filter(housing, status == "R", !is.na(host_ID), year(date) == 2021) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  mutate(tier = "All", .before = host_ID) |> 
  bind_rows(daily_for_rev)

host_deciles <-
  daily_for_rev |> 
  group_by(tier) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) |> 
  select(-all) |> 
  pivot_longer(-tier, names_to = "percentile", values_to = "value") |> 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) |> 
  mutate(perfect_distribution = 0.1,
         decile = rep(1:10, 6),
         dummy_1 = perfect_distribution,
         dummy_2 = value) |> 
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) |> 
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") |> 
  mutate(position = as.numeric(position), 
         display_val = scales::percent(value, .1)) |> 
  group_by(tier, position) |> 
  mutate(absolute_val = slider::slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                          .after = 9)) |> 
  ungroup() |> 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

host_rev_data <- 
  daily_for_rev |> 
  filter(tier == "All") |> 
  arrange(-rev) |> 
  summarize(
    pct_1 = quantile(rev, 0.99),
    pct_10 = quantile(rev, 0.9),
    n_1 = round(n() / 10),
    rev_1 = sum(rev[rev >= pct_1]) / sum(rev),
    rev_10 = sum(rev[rev >= pct_10]) / sum(rev),
  )

host_top_10_pct <- scales::percent(host_rev_data$rev_10, 0.1)
host_top_1_pct <- scales::percent(host_rev_data$rev_1, 0.1)
host_top_1_n <- scales::percent(host_rev_data$n_1, 0.1)

ml_active <- 
  daily |> 
  filter(housing, str_starts(property_ID, "ab-"), status %in% c("A", "R")) |> 
  group_by(tier, date) |> 
  summarize(pct = mean(multi))

ml_active <- 
  daily |> 
  filter(housing, str_starts(property_ID, "ab-"), status %in% c("A", "R")) |> 
  group_by(date) |> 
  summarize(pct = mean(multi)) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(ml_active)

ml_pct_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status == "R") |> 
  filter(str_starts(property_ID, "ab-")) |> 
  summarize(pct = mean(multi)) |> 
  pull(pct) |> 
  scales::percent(0.1)

ml_rev_pct_2021 <- 
  daily |> 
  filter(housing, year(date) == 2021, status == "R") |> 
  filter(str_starts(property_ID, "ab-")) |> 
  summarize(rev_pct = sum(price[multi]) / sum(price)) |> 
  pull(rev_pct) |> 
  scales::percent(0.1)


# Save output -------------------------------------------------------------

qs::qsavem(active_all_avg_2021, active_non_housing_2021, active_avg_2021,
           hosts_avg_2021, rev_total_2021, rev_avg_2021, rev_med_2021,
           active_avg_2019, active_change_pct, hosts_avg_2019, hosts_change_pct,
           rev_total_2019, rev_change_pct, rev_avg_2019, rev_med_2019,
           rev_avg_change_pct, rev_med_change_pct, active_daily_tiers,
           eh_pct_2021, eh_pct_2019, revenue_colour, host_deciles,
           host_top_10_pct, host_top_1_pct, host_top_1_n, ml_active,
           ml_pct_2021, ml_rev_pct_2021,
           file = "output/data/ch_1.qsm", nthreads = future::availableCores())
