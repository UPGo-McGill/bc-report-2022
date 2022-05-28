#### Analysis for chapter 2 ####################################################

library(tidyverse)
library(lubridate)
library(slider)
library(tsibble)
library(feasts)
library(fable)
library(sf)

qs::qload("output/data/data_processed.qsm", nthreads = future::availableCores())
qs::qload("output/data/FREH_model.qsm", nthreads = future::availableCores())
cmhc <- qs::qread("output/data/cmhc.qs", nthreads = future::availableCores())
qs::qload("output/model_chapter.qsm", nthreads = future::availableCores())


# STR-induced housing loss ------------------------------------------------

FREH_total <- 
  daily |> 
  filter(housing, date >= "2016-01-01") |> 
  group_by(tier, date) |>  
  summarize(FREH = sum(FREH_3), .groups = "drop") |> 
  filter(day(date) == 1)

FREH_total <- 
  daily |> 
  filter(housing, date >= "2016-01-01") |> 
  group_by(date) |>  
  summarize(FREH = sum(FREH_3), .groups = "drop") |> 
  filter(day(date) == 1) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(FREH_total)

GH_total <-
  GH |> 
  st_drop_geometry() |> 
  group_by(tier, date) |> 
  summarize(GH = sum(housing_units), .groups = "drop") |> 
  mutate(GH = slide_dbl(GH, mean, .before = 29))

GH_total <-
  GH |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop") |> 
  mutate(GH = slide_dbl(GH, mean, .before = 29)) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(GH_total)

housing_loss <-
  FREH_total |> 
  left_join(GH_total, by = c("tier", "date")) |> 
  rename(`Entire home/apt` = FREH, `Private room` = GH) |> 
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") |> 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

freh_2021 <- 
  daily |> 
  filter(housing, date == "2021-12-01") |> 
  summarize(FREH = sum(FREH_3)) |> 
  pull(FREH) |> 
  scales::comma(10)

gh_units_2021 <- 
  GH |> 
  st_drop_geometry() |> 
  filter(date == "2021-12-31") |> 
  summarize(units = sum(housing_units)) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_2021 <- 
  housing_loss |> 
  filter(tier == "All", date == "2021-12-01") |> 
  pull(`Housing units`) |> 
  sum() |> 
  scales::comma(10)
  
active_decline_pct_2019_2021 <- 
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2021)) |> 
  count(year = year(date)) |> 
  summarize(pct = (n[year == "2021"] - n[year == "2019"]) / 
              n[year == "2019"]) |> 
  pull(pct) |> 
  abs() |> 
  scales::percent(0.1)

housing_loss_2019 <- 
  housing_loss |> 
  filter(tier == "All", date == "2019-12-01") |> 
  pull(`Housing units`) |> 
  sum() |> 
  scales::comma(10)

housing_loss_decline_pct_2019_2021 <- 
  housing_loss |> 
  filter(tier == "All", date %in% as.Date(c("2019-12-01", "2021-12-01"))) |> 
  group_by(date) |> 
  summarize(units = sum(`Housing units`)) |> 
  summarize(pct = (units[2] - units[1]) / units[1]) |> 
  pull(pct) |> 
  abs() |> 
  scales::percent(0.1)

# Get daily housing loss
housing_loss_daily <- 
  daily |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(tier, date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

housing_loss_daily <- 
  daily |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3)) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(housing_loss_daily)

GH_daily <- 
  GH |> 
  st_drop_geometry() |> 
  group_by(tier, date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

GH_daily <- 
  GH |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop") |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(GH_daily)

housing_loss_daily <- 
  housing_loss_daily |> 
  left_join(GH_daily, by = c("tier", "date")) |> 
  mutate(units = FREH + GH) |> 
  select(tier, date, units)

# Create monthly time series
housing_loss_monthly_series <- 
  housing_loss_daily |> 
  tsibble::as_tsibble(key = tier, index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  group_by(tier) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model <- 
  housing_loss_monthly_series |> 
  filter(yearmon <= yearmonth("2020-02")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), NAIVE(season_adjust)))

# Create housing loss forecast
housing_loss_forecast <-
  housing_loss_model |> 
  forecast(h = "24 months") |> 
  as_tibble() |> 
  select(tier, yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  left_join(housing_loss_forecast, by = c("tier", "yearmon"))

# Integrate forecast into daily data
housing_loss_daily <- 
  housing_loss_daily |> 
  group_by(tier) |> 
  mutate(units_trend = slider::slide_dbl(units, ~.x[1], .before = 366,
                .complete = TRUE)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, .before = 6, 
                                         .complete = TRUE)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_series, -units), 
            by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(yearmon:units_trend_month))

housing_loss_cc_end_2021 <- 
  housing_loss_monthly_series |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_cc_trend_end_2021 <- 
  housing_loss_monthly_series |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  pull(units_trend_month) |> 
  scales::comma(10)

housing_loss_cc_dif_pct_end_2021 <- 
  housing_loss_monthly_series |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  summarize(dif = units_trend_month / units - 1) |> 
  pull(dif) |> 
  scales::percent(0.1)


# The impact of dedicated STRs on residential rents in BC -----------------

rent_tier_1 <- 
  cmhc$rent |> 
  filter(year == 2021, !is.na(tier)) |> 
  group_by(tier) |> 
  summarize(total = mean(total, na.rm = TRUE)) |> 
  mutate(total = scales::dollar(total, 10))

rent_tier <- rent_tier_1$total
names(rent_tier) <- rent_tier_1$tier



cmhc$rent |> 
  mutate(tier = "All") |> 
  bind_rows(cmhc$rent) |> 
  select(tier, year, neighbourhood, total) |> 
  filter(!is.na(tier)) |> 
  ggplot(aes(year, total, colour = tier)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_color_brewer(palette = "Accent", guide = "none") +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank())






find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- (q3 - q1) * 1.5
  which(x < q1 - iqr | x > q3 + iqr)
}

figure_CHAPNUM_1_fun <- function(regular = "", condensed = "") {
  
  cmhc_str[-find_outliers(cmhc_str$freh_p_dwellings), ] |> 
    mutate(year = year + 2016) |> 
    filter(!is.na(tier)) |> 
    ggplot(aes(freh_p_dwellings, total_rent)) +
    geom_point(aes(color = year), size = 0.5) +
    geom_smooth(color = "black", se = FALSE, method = "lm", level = 0.95) +
    facet_wrap(~tier, scales = "free")
  
}










# Save output -------------------------------------------------------------

qs::qsavem(housing_loss, freh_2021, gh_units_2021, housing_loss_2021,
           active_decline_pct_2019_2021, housing_loss_2019,
           housing_loss_decline_pct_2019_2021, housing_loss_daily,
           housing_loss_cc_end_2021, housing_loss_cc_trend_end_2021,
           housing_loss_cc_dif_pct_end_2021,
           file = "output/data/ch_2.qsm", nthreads = future::availableCores())


