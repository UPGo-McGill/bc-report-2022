#### Analysis for chapter 1 ####################################################

library(tidyverse)
library(lubridate)
library(slider)
library(tsibble)
library(feasts)
library(fable)

qs::qload("output/data/data_processed.qsm", nthreads = future::availableCores())
qs::qload("output/data/geometry.qsm", nthreads = future::availableCores())

col_palette <-
  c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
    "#AE7673")


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

active_2022 <- 
  daily |> 
  filter(housing, date >= "2022-04-01", status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

active_change_pct_2022 <- 
  daily |> 
  filter(housing, year(date) %in% 2021:2022, month(date) == 4, 
         status %in% c("A", "R")) |> 
  count(year = year(date)) |> 
  summarize(pct = (max(n) - min(n)) / min(n)) |> 
  pull() |> 
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
  

# Figure 1 ----------------------------------------------------------------

fig_1 <- 
  active_daily_tiers |> 
  group_by(tier) |> 
  mutate(n = slider::slide_dbl(n, mean, .before = 13)) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  ungroup() |> 
  filter(date >= "2017-07-01", !is.na(tier)) |> 
  ggplot() +
  geom_line(aes(date, n, colour = tier)) +
  facet_wrap(~tier, scales = "free_y") +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_x_date(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_1.png", fig_1, width = 9, height = 5)


# Table 1 -----------------------------------------------------------------

tab_1 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2021)) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, dwellings, name),
            by = "CSDUID") |>
  filter(!is.na(name)) |> 
  group_by(name) |> 
  summarize(active_2021 = sum(year(date) == 2021) / 365,
            active_2019 = sum(year(date) == 2019) / 365,
            active_growth = (active_2021 - active_2019) / active_2019,
            active_pct = active_2021 / mean(dwellings),
            rev_2021 = sum(price[status == "R" & year(date) == 2021]),
            rev_2019 = sum(price[status == "R" & year(date) == 2019]),
            rev_growth = (rev_2021 - rev_2019) / rev_2019,
            .groups = "drop") |> 
  arrange(-active_2021) |> 
  slice(1:10)

tab_1 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2021)) |> 
  summarize(active_2021 = sum(year(date) == 2021) / 365,
            active_2019 = sum(year(date) == 2019) / 365,
            active_growth = (active_2021 - active_2019) / active_2019,
            active_pct = active_2021 / sum(CSD$dwellings, na.rm = TRUE),
            rev_2021 = sum(price[status == "R" & year(date) == 2021]),
            rev_2019 = sum(price[status == "R" & year(date) == 2019]),
            rev_growth = (rev_2021 - rev_2019) / rev_2019,
            .groups = "drop") |> 
  mutate(name = "British Columbia", .before = active_2021) |> 
  bind_rows(tab_1)

tab_1 |> 
  select(-active_2019, -rev_2019) |> 
  mutate(active_2021 = scales::comma(active_2021, 10),
         active_growth = scales::percent(active_growth, 0.1),
         active_pct = scales::percent(active_pct, 0.1),
         rev_2021 = scales::dollar(rev_2021, 0.1, scale = 1/1000000, 
                                   suffix = " million"),
         rev_growth = scales::percent(rev_growth, 0.1)) |> 
  gt::gt()


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

revenue_colour <- col_palette[c(5, 2, 1, 3, 4, 8, 6, 7)]

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
host_top_1_n <- scales::comma(host_rev_data$n_1, 10)

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



# Figure 2 ----------------------------------------------------------------

fig_2 <- 
  host_deciles |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  facet_wrap(~ tier, nrow = 2) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_2.png", fig_2, width = 9, height = 5)


# Figure 3 ----------------------------------------------------------------

fig_3 <- 
  ml_active |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  group_by(tier) |> 
  mutate(n = slider::slide_dbl(pct, mean, .before = 13)) |> 
  ungroup() |> 
  filter(date >= "2017-07-01", !is.na(tier)) |> 
  ggplot() +
  geom_line(aes(date, n, colour = tier)) +
  facet_wrap(~tier) +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_x_date(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_3.png", fig_3, width = 9, height = 5)


# Growth trends: pre- and post-Covid --------------------------------------

daily_variation <-
  daily |> 
  filter(housing, status != "B", date != "2020-02-29") |> 
  filter(str_starts(property_ID, "ab-")) |>
  count(tier, date) |> 
  mutate(n = slide_dbl(n, ~{(.x[366] - .x[1]) / .x[1]}, .before = 365, 
                       .complete = FALSE)) |> 
  filter(!is.na(n), !is.infinite(n)) |> 
  filter(date >= as.Date("2017-06-01") + years(1))

daily_variation <-
  daily |> 
  filter(housing, status != "B", date != "2020-02-29") |> 
  filter(str_starts(property_ID, "ab-")) |>
  count(date) |> 
  mutate(n = slide_dbl(n, ~{(.x[366] - .x[1]) / .x[1]}, .before = 365, 
                       .complete = FALSE)) |> 
  filter(!is.na(n), !is.infinite(n)) |> 
  filter(date >= as.Date("2017-06-01") + years(1)) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(daily_variation)

active_change_pct_2017 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2016, 2017)) |> 
  count(year = year(date)) |> 
  summarize(pct = n[year == 2017] / sum(n)) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2018 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2017, 2018)) |> 
  count(year = year(date)) |> 
  summarize(pct = (n[year == 2018] - n[year == 2017]) / n[year == 2018]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2019 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2018, 2019)) |> 
  count(year = year(date)) |> 
  summarize(pct = (n[year == 2019] - n[year == 2018]) / n[year == 2018]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2020 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2020)) |> 
  count(year = year(date)) |> 
  summarize(pct = (n[year == 2020] - n[year == 2019]) / n[year == 2019]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2021 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2020, 2021)) |> 
  count(year = year(date)) |> 
  summarize(pct = (n[year == 2021] - n[year == 2020]) / n[year == 2020]) |> 
  pull(pct) |> 
  scales::percent(0.1)


# Figure 4 ----------------------------------------------------------------

fig_4 <- 
  daily_variation |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  group_by(tier) |> 
  mutate(n = slide_dbl(n, mean, .before = 20, .complete = TRUE)) |> 
  ungroup() |> 
  ggplot(aes(date, n, colour = tier)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(lwd = 1, na.rm = TRUE) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(-0.6, 0.6), 
                     labels = scales::percent) +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_4.png", fig_4, width = 9, height = 5)


# Trend analysis ----------------------------------------------------------

# Get daily reservations and prices
reservations_and_prices <- 
  daily |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(tier, date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

reservations_and_prices <- 
  daily |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop") |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(reservations_and_prices)

# Create monthly time series
monthly_series <- 
  reservations_and_prices |> 
  tsibble::as_tsibble(key = tier, index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  group_by(tier) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) %>% 
  relocate(price, .after = res)

# Create reservations model
reservations_model <- 
  monthly_series |> 
  filter(yearmon <= yearmonth("2020-01")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create CC reservations model
reservations_model_CC <- 
  monthly_series |> 
  filter(tier == "CC", yearmon <= yearmonth("2019-10")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

reservations_model$res[reservations_model$tier == "CC"] <- 
  reservations_model_CC$res

# Create reservations forecast
reservations_forecast <-
  reservations_model |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(tier, yearmon, res_trend_month = .mean)

# Create price model
price_model <- 
  monthly_series |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast <- 
  price_model |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(tier, yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series <- 
  monthly_series |>  
  left_join(reservations_forecast, by = c("tier", "yearmon")) |> 
  left_join(price_forecast, by = c("tier", "yearmon"))

# Integrate forecasts into daily data
reservations_and_prices <-
  reservations_and_prices |> 
  mutate(prepan = (tier != "CC" & date >= "2019-02-01" & date <= "2020-01-31") |
           (tier == "CC" & date >= "2018-11-01" & date <= "2019-10-31")) |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(tier, month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series, -res, -price), by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  group_by(tier) |>
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  ungroup() |> 
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))
  
res_all <- 
  reservations_and_prices |> 
  filter(tier != "All") |> 
  group_by(date) |> 
  summarize(tier = "All",
            res_new = sum(res, na.rm = TRUE),
            res_trend_new = sum(res_trend, na.rm = TRUE)) |> 
  mutate(res_trend_new = if_else(date >= "2020-03-01", res_trend_new, NA_real_))

reservations_and_prices <- 
  reservations_and_prices |> 
  left_join(res_all, by = c("tier", "date")) |> 
  mutate(res = coalesce(res_new, res),
         res_trend = coalesce(res_trend_new, res_trend)) |> 
  select(-res_new, -res_trend_new)

covid_res_dif <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "All") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "All") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct <-
  {parse_number(covid_res_total) / (parse_number(covid_res_dif) + 
                                       parse_number(covid_res_total))} |> 
  scales::percent(0.1)

covid_res_trend <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "All") |> 
  summarize(res_tot = sum(res_trend)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_cc_res_dif <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "CC") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif)

covid_cc_res_total <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "CC") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot)

covid_cc_res_pct <- 
  {covid_cc_res_total / (covid_cc_res_dif + covid_cc_res_total)} |> 
  scales::percent(0.1)

covid_res_res_dif <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "RES") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif)

covid_res_res_total <-
  reservations_and_prices |> 
  filter(date >= "2020-03-01", tier == "RES") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot)

covid_res_res_pct <- 
  {covid_res_res_total / (covid_res_res_dif + covid_res_res_total)} |> 
  scales::percent(0.1)


# Figure 5 ----------------------------------------------------------------

fig_5 <- 
  reservations_and_prices |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  group_by(tier) |> 
  mutate(res = slide_dbl(res, mean, .before = 6, .complete = TRUE)) |> 
  ungroup() |> 
  mutate(res_trend = if_else(date <= "2020-03-14", NA_real_, res_trend)) |> 
  select(tier, date, res, res_trend) |> 
  pivot_longer(-c(tier, date)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    tier == "All" & date == "2019-07-05" & name == "res" ~ 
      "Actual reservations", 
    tier == "All" & date == "2020-08-05" & name == "res_trend" ~ 
      "Expected reservations",
    TRUE ~ NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = res, ymax = res_trend, group = 1),
              data = {
                reservations_and_prices |> 
                  mutate(tier = case_when(
                    tier == "All" ~ "All",
                    tier == "CA" ~ "Mid-sized cities",
                    tier == "CC" ~ "Core cities",
                    tier == "CMA" ~ "Large regions",
                    tier == "NU" ~ "Non-urban",
                    tier == "RES" ~ "Resort towns")) |> 
                  group_by(tier) |> 
                  mutate(res = slider::slide_dbl(res, mean, .before = 6, 
                                                 .complete = TRUE)) |> 
                  ungroup() |> 
                  mutate(res_trend = if_else(date <= "2020-03-14", NA_real_, 
                                             res_trend))}, 
              fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 0.5) +
  geom_label(aes(date, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual reservations", "Expected reservations"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tier, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_5.png", fig_5, width = 9, height = 5)


# Figure 6 ----------------------------------------------------------------

fig_6 <-
  reservations_and_prices |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  group_by(tier) |> 
  mutate(price = slide_dbl(price, mean, .before = 6, .complete = TRUE)) |> 
  ungroup() |> 
  mutate(price_trend = if_else(date <= "2020-03-14", NA_real_, price_trend)) |> 
  select(tier, date, price, price_trend) |> 
  pivot_longer(-c(tier, date)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    tier == "All" & date == "2018-11-05" & name == "price" ~ 
      "Actual price", 
    tier == "All" & date == "2021-01-05" & name == "price_trend" ~ 
      "Expected price",
    TRUE ~ NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = price, ymax = price_trend, group = 1),
              data = {
                reservations_and_prices |> 
                  mutate(tier = case_when(
                    tier == "All" ~ "All",
                    tier == "CA" ~ "Mid-sized cities",
                    tier == "CC" ~ "Core cities",
                    tier == "CMA" ~ "Large regions",
                    tier == "NU" ~ "Non-urban",
                    tier == "RES" ~ "Resort towns")) |> 
                  group_by(tier) |> 
                  mutate(price = slider::slide_dbl(price, mean, .before = 6, 
                                                   .complete = TRUE)) |> 
                  ungroup() |> 
                  mutate(price_trend = if_else(
                    date <= "2020-03-14", NA_real_, price_trend))}, 
              fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 0.5) +
  geom_label(aes(date, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::dollar) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual price", "Expected price"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tier, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_6.png", fig_6, width = 9, height = 5)


# Save output -------------------------------------------------------------

qs::qsavem(active_all_avg_2021, active_non_housing_2021, active_avg_2021,
           hosts_avg_2021, rev_total_2021, rev_avg_2021, rev_med_2021,
           active_avg_2019, active_change_pct, hosts_avg_2019, hosts_change_pct,
           rev_total_2019, rev_change_pct, rev_avg_2019, rev_med_2019,
           rev_avg_change_pct, rev_med_change_pct, active_2022, 
           active_change_pct_2022, active_daily_tiers, eh_pct_2021, eh_pct_2019, 
           revenue_colour, host_deciles, host_top_10_pct, host_top_1_pct, 
           host_top_1_n, ml_active, ml_pct_2021, ml_rev_pct_2021, 
           daily_variation, active_change_pct_2018, active_change_pct_2019, 
           active_change_pct_2020, active_change_pct_2021, 
           reservations_and_prices, covid_res_dif, covid_res_total, 
           covid_res_pct, covid_res_trend, covid_cc_res_pct, covid_res_res_pct,
           file = "output/data/ch_1.qsm", nthreads = future::availableCores())
