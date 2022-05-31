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
qs::qload("output/data/model_chapter.qsm", nthreads = future::availableCores())


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
  filter(status != "B") |> 
  group_by(tier, date) |> 
  summarize(GH = sum(housing_units), .groups = "drop") |> 
  mutate(GH = slide_dbl(GH, mean, .before = 29))

GH_total <-
  GH |> 
  st_drop_geometry() |> 
  filter(status != "B") |> 
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
  filter(date == "2021-12-31", status != "B") |> 
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


# Housing loss model ------------------------------------------------------

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
  filter(status != "B") |> 
  group_by(tier, date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

GH_daily <- 
  GH |> 
  st_drop_geometry() |> 
  filter(status != "B") |> 
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
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast <-
  housing_loss_model |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(tier, yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  full_join(housing_loss_forecast, by = c("tier", "yearmon"))

# Integrate forecast into daily data
housing_loss_daily_model <-
  housing_loss_daily |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(tier, month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  full_join(select(housing_loss_monthly_series, -units), 
            by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month)) |> 
  group_by(tier) |>
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  ungroup() |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

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

# A regression model of dedicated STRs and residential rent ---------------

rent_tier_df <- 
  cmhc$rent |> 
  filter(year == 2021, !is.na(tier)) |> 
  mutate(tier = if_else(tier %in% c("NU", "RES"), "RES/NU", tier)) |> 
  group_by(tier) |> 
  summarize(total = mean(total, na.rm = TRUE)) |> 
  mutate(total = scales::dollar(total, 10))

rent_tier <- rent_tier_df$total
names(rent_tier) <- rent_tier_df$tier
cmhc_str <- select(cmhc_str, -geometry)

model_iv_coef_dollar <- scales::dollar(model$coefficients[["iv"]], 0.01)
model_year_coef_dollar <- scales::dollar(model$coefficients[["year"]], 0.01)
model_renter_coef_dollar <- scales::dollar(model$coefficients[["renter_pct"]], 
                                           0.01)


# The burden of STRs on BC renter households ------------------------------

rent_2016_2021 <-
  cmhc$rent |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_2016_2021_dollar <- 
  scales::dollar(rent_2016_2021, 0.1, scale = 1/1000000000, suffix = " billion")

rent_str_2016_2021 <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2016_2021 <- (rent_str_2016_2021 / rent_2016_2021) |> 
  scales::percent(0.1)

overpaid_2016_2021 <- 
  rent_str_2016_2021 |> 
  scales::dollar(0.1, scale = 1/1000000000, suffix = " billion")

rent_2019 <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019 <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019 <- (rent_str_2019 / rent_2019) |> scales::percent(0.1)

rent_change_table_raw <- 
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_raw <- 
  rent_change_table_raw |> 
  mutate(tier = "All") |> 
  bind_rows(rent_change_table_raw)

rent_change_table <- 
  rent_change_table_raw |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year, tier) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop")

rent_change_table_year <- 
  rent_change_table_raw |> 
  group_by(year, tier) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop") |> 
  filter(year != 0) |> 
  mutate(year = 2016 + year, raw_rent = med_rent - med_str)

str_incr_2017_2019 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

rent_month_2020 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2020") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2020 <- 
  rent_change_table |> 
  filter(tier == "All", year == "2020") |> 
  pull(mean_str) |> 
  abs() |> 
  scales::dollar(01)


# Trend analysis: STR rent burden -----------------------------------------

# Create monthly time series
housing_loss_monthly_series_2023 <- 
  housing_loss_daily |> 
  select(-units_trend) |> 
  tsibble::as_tsibble(key = tier, index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  group_by(tier) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_2023 <- 
  housing_loss_monthly_series_2023 |> 
  filter(yearmon <= yearmonth("2020-02")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), NAIVE(season_adjust)))

# Create housing loss forecast
housing_loss_forecast_2023 <-
  housing_loss_model_2023 |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(tier, yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_2023 <- 
  housing_loss_monthly_series_2023 |>  
  full_join(housing_loss_forecast_2023, by = c("tier", "yearmon"))

# Integrate forecast into daily data
housing_loss_daily_2023 <- 
  housing_loss_daily |> 
  select(-units_trend) |> 
  group_by(tier) |> 
  mutate(units_trend = slider::slide_dbl(units, ~.x[1], .before = 366,
                                         .complete = TRUE)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, .before = 6, 
                                         .complete = TRUE)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  full_join(select(housing_loss_monthly_series_2023, -units), 
            by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(yearmon:units_trend_month))

housing_loss_daily_2023 |> 
  filter(date >= "2022-03-01")
  



# Save output -------------------------------------------------------------

qs::qsavem(housing_loss, freh_2021, gh_units_2021, housing_loss_2021,
           active_decline_pct_2019_2021, housing_loss_2019,
           housing_loss_decline_pct_2019_2021, housing_loss_daily,
           housing_loss_cc_end_2021, housing_loss_cc_trend_end_2021,
           housing_loss_cc_dif_pct_end_2021, rent_tier, cmhc, cmhc_str,
           model, model_iv_coef_dollar, model_year_coef_dollar, 
           model_renter_coef_dollar, rent_2016_2021_dollar,
           rent_str_pct_2016_2021, overpaid_2016_2021, rent_str_pct_2019,
           str_incr_2017_2019, rent_month_2017_2019, str_incr_month_2017_2019,
           str_incr_2020, rent_month_2020, str_incr_month_2020,
           rent_change_table, rent_change_table_year,
           file = "output/data/ch_2.qsm", nthreads = future::availableCores())


