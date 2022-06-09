#### Analysis for chapter 3 ####################################################

library(tidyverse)
library(lubridate)
library(slider)
library(tsibble)
library(feasts)
library(fable)
library(sf)
library(osmdata)

qs::qload("output/data/data_processed.qsm", nthreads = future::availableCores())
qs::qload("output/data/model_chapter.qsm", nthreads = future::availableCores())
qs::qload("output/data/geometry.qsm", nthreads = future::availableCores())

case_studies <- c("Vancouver", "Richmond", "Nanaimo", "Kelowna", "Summerland")

model_iv_coef_dollar <- scales::dollar(model$coefficients[["iv"]], 0.01)
model_year_coef_dollar <- scales::dollar(model$coefficients[["year"]], 0.01)
model_renter_coef_dollar <- scales::dollar(model$coefficients[["renter_pct"]], 
                                           0.01)


# Vancouver ---------------------------------------------------------------

property_V <- 
  property |> 
  filter(housing, city == "Vancouver")

daily_V <- 
  daily |> 
  filter(housing, city == "Vancouver")

active_avg_2021_V <- 
  daily_V |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2021_V <- 
  daily_V |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2021_V <- 
  daily_V |> 
  filter(year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021_V <- 
  daily_V |> 
  filter(year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021_V <- scales::dollar(rev_host_2021_V$avg, 100)
rev_med_2021_V <- scales::dollar(rev_host_2021_V$med, 100)

active_avg_2019_V <- 
  daily_V |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2019_V <- 
  daily_V |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2019_V <- 
  daily_V |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2019_V <- 
  daily_V |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_V <- scales::dollar(rev_host_2019_V$avg, 100)
rev_med_2019_V <- scales::dollar(rev_host_2019_V$med, 100)

active_daily_V <- 
  daily |> 
  filter(housing, status %in% c("A", "R"), tier == "CC") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(group = if_else(name == "Vancouver", "Vancouver", 
                         "Central city average")) |> 
  group_by(date, group) |> 
  summarize(n_pct = sum(n, na.rm = TRUE) / sum(dwellings, na.rm = TRUE),
            .groups = "drop") |> 
  select(date, group, n_pct) |> 
  group_by(group) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-06-01", !is.na(group))

listings_pct_V <- 
  property_V |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_join(select(CT, GeoUID, dwellings = Dwellings)) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(pct = n() / mean(dwellings))

streets_V <- 
  (getbb("Vancouver British Columbia") * c(1.01, 0.99, 0.99, 1.01)) |>  
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

streets_V <-
  rbind(
    streets_V$osm_polygons |>  st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets_V$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32610) |> 
  st_set_agr("constant") |> 
  st_intersection(filter(CSD, name == "Vancouver"))

streets_V <-
  streets_V |>  
  select(osm_id, name, highway, geometry) |> 
  filter(highway %in% c("primary", "secondary", "service", "residential",
                        "tertiary", "unclassified", "motorway", 
                        "motorway_link"))

streets_V_2 <- 
  streets_V |> 
  filter(highway %in% c("residential", "service", "unclassified"))

streets_V <- 
  streets_V |> 
  filter(highway %in% c("primary", "secondary", "tertiary", "motorway", 
                        "motorway_link"))

# Get daily reservations and prices
reservations_and_prices_V <- 
  daily_V |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

# Create monthly time series
monthly_series_V <- 
  reservations_and_prices_V |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) |> 
  relocate(price, .after = res)

# Create reservations model
reservations_model_V <- 
  monthly_series_V |> 
  filter(yearmon <= yearmonth("2019-10")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create reservations forecast
reservations_forecast_V <-
  reservations_model_V |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, res_trend_month = .mean)

# Create price model
price_model_V <- 
  monthly_series_V |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast_V <- 
  price_model_V |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_V <- 
  monthly_series_V |>  
  left_join(reservations_forecast_V, by = "yearmon") |> 
  left_join(price_forecast_V, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_V <-
  reservations_and_prices_V |> 
  mutate(prepan = date >= "2018-11-01" & date <= "2019-10-31") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series_V, -res, -price), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))

covid_res_dif_V <-
  reservations_and_prices_V |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total_V <-
  reservations_and_prices_V |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct_V <-
  {parse_number(covid_res_total_V) / (parse_number(covid_res_dif_V) + 
                                        parse_number(covid_res_total_V))} |> 
  scales::percent(0.1)

covid_price_pct_V <-
  reservations_and_prices_V |> 
  filter(date >= "2020-03-01") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)

# Get daily housing loss
housing_loss_daily_V <- 
  daily_V |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_daily_V <- 
  GH |> 
  filter(status != "B") |>
  st_filter(filter(CSD, name == "Vancouver")) |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

housing_loss_daily_V <- 
  housing_loss_daily_V |> 
  left_join(GH_daily_V, by = "date") |> 
  mutate(units = FREH + GH) |>
  select(date, units)

commercial_pct_V <-
  daily_V |> 
  filter(date >= "2017-06-01", status != "B") |> 
  count(date) |> 
  left_join(housing_loss_daily_V, by = "date") |> 
  mutate(pct = units / n) |> 
  filter(date >= "2018-01-01") |> 
  summarize(mean = mean(pct, na.rm = TRUE)) |> 
  pull() |> 
  scales::percent(0.1)

# Create monthly time series
housing_loss_monthly_series_V <- 
  housing_loss_daily_V |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_V <- 
  housing_loss_monthly_series_V |> 
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast_V <-
  housing_loss_model_V |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_V <- 
  housing_loss_monthly_series_V |>  
  full_join(housing_loss_forecast_V, by = "yearmon")

# Add decay to growth rate
housing_loss_monthly_decay_V <-
  housing_loss_monthly_series_V |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into daily data
housing_loss_daily_model_V <-
  housing_loss_daily_V |> 
  add_row(date = as.Date(as.Date("2022-04-01", origin = "1970-01-01"):
                           as.Date("2023-12-31", origin = "1970-01-01"), 
                         origin = "1970-01-01")) |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_decay_V, -units), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

# Rent calculations
rent_str_2016_2021_V <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Vancouver") |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

overpaid_2016_2021_V <- 
  rent_str_2016_2021_V |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rent_2019_V <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Vancouver") |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019_V <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Vancouver") |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019_V <- (rent_str_2019_V / rent_2019_V) |> scales::percent(0.1)

rent_change_table_raw_V <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Vancouver") |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_V <- 
  rent_change_table_raw_V |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
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

str_incr_2017_2019_V <- 
  rent_change_table_V |> 
  filter(year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019_V <- 
  rent_change_table_V |> 
  filter(year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019_V <- 
  rent_change_table_V |> 
  filter(year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020_V <- 
  rent_change_table_V |> 
  filter(year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

# Rent projections
housing_loss_2023_V <- 
  housing_loss_daily_model_V |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023_V <-
  housing_loss_daily_model_V |> 
  filter((date == "2023-12-31" | date == "2021-12-31")) |> 
  summarize(dif = (units_trend[2] - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)

rent_inc_monthly_2021_2023_V <-
  rent_change_table_raw_V |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_V |> 
      filter((date == "2023-12-31" | date == "2021-12-31")) |> 
      group_by(tier = "CC") |> 
      summarize(units = units[1], units_trend = units_trend[2], 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)

rent_inc_annual_2021_2023_V <- 
  (parse_number(rent_inc_monthly_2021_2023_V) * 12) |> 
  scales::dollar(1)

housing_loss_2023_raw_V <- 
  housing_loss_daily_model_V |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend)

rent_change_2023_table_V <- 
  rent_change_table_raw_V |> 
  filter(year == 5) |> 
  mutate(housing_loss = iv * renters / 100) |> 
  mutate(new_housing_loss = (housing_loss_2023_raw_V - sum(housing_loss)) * 
           (housing_loss / sum(housing_loss))) |> 
  mutate(str_2023 = new_housing_loss / renters * 100 * 
           model$coefficients[["iv"]],
         year_2023 = model$coefficients[["year"]]) |> 
  select(neighbourhood, total_rent, new_housing_loss, str_2023, year_2023) |> 
  mutate(rent_2023_pct = str_2023 / (total_rent + year_2023 * 2),
         rent_inc_pct = str_2023 / (str_2023 + year_2023 * 2))

# Photo prep
library(matchr)
qs::qload("data/matches_raw.qsm")
ltr <- qs::qread("data/ltr_processed.qs", nthreads = future::availableCores())

first_photo_pair <- 
  cl_matches |> 
  filter(confirmation == "match") |> 
  filter(x_name == "/Volumes/Data 2/Scrape photos/vancouver/ab/ab-18753643.jpg")

second_photo_pair <- 
  cl_matches |> 
  filter(confirmation == "match") |> 
  filter(x_name == "/Volumes/Data 2/Scrape photos/vancouver/ab/ab-10972081.jpg")

titles <- list(
  
  first_photo_pair$x_name |> 
    str_extract('ab-.*(?=\\.jpg)') |> 
    (\(x) filter(property, property_ID == x))() |> 
    pull(listing_title),
  
  first_photo_pair$y_name |> 
    str_extract('cl-.*(?=-[:digit:]\\.jpg)') |> 
    (\(x) filter(ltr, id == x))() |> 
    slice(1) |> 
    pull(title) |> 
    str_remove(' \\|.*'),
  
  second_photo_pair$x_name |> 
    str_extract('ab-.*(?=\\.jpg)') |> 
    (\(x) filter(property, property_ID == x))() |> 
    pull(listing_title),
  
  second_photo_pair$y_name |> 
    str_extract('cl-.*(?=-[:digit:]\\.jpg)') |> 
    (\(x) filter(ltr, id == x))() |> 
    slice(1) |> 
    pull(title) |> 
    str_remove(' - apts.*')
)




# Richmond ----------------------------------------------------------------

property_R <- 
  property |> 
  filter(housing, city == "Richmond")

daily_R <- 
  daily |> 
  filter(housing, city == "Richmond")

active_avg_2021_R <- 
  daily_R |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2021_R <- 
  daily_R |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2021_R <- 
  daily_R |> 
  filter(year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021_R <- 
  daily_R |> 
  filter(year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021_R <- scales::dollar(rev_host_2021_R$avg, 100)
rev_med_2021_R <- scales::dollar(rev_host_2021_R$med, 100)

active_avg_2019_R <- 
  daily_R |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2019_R <- 
  daily_R |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2019_R <- 
  daily_R |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2019_R <- 
  daily_R |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_R <- scales::dollar(rev_host_2019_R$avg, 100)
rev_med_2019_R <- scales::dollar(rev_host_2019_R$med, 100)

active_daily_R <- 
  daily |> 
  filter(housing, status %in% c("A", "R"), tier == "CMA") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(group = if_else(name == "Richmond", "Richmond", 
                         "Large urban region average")) |> 
  group_by(date, group) |> 
  summarize(n_pct = sum(n, na.rm = TRUE) / sum(dwellings, na.rm = TRUE),
            .groups = "drop") |> 
  select(date, group, n_pct) |> 
  group_by(group) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-06-01", !is.na(group))

listings_pct_R <- 
  property_R |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_join(select(CT, GeoUID, dwellings = Dwellings)) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(pct = n() / mean(dwellings))

streets_R <- 
  (getbb("Richmond British Columbia") * c(1.01, 0.99, 0.99, 1.01)) |>  
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

streets_R <-
  rbind(
    streets_R$osm_polygons |>  st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets_R$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32610) |> 
  st_set_agr("constant") |> 
  st_intersection(filter(CSD, name == "Richmond"))

streets_R <-
  streets_R |>  
  select(osm_id, name, highway, geometry) |> 
  filter(highway %in% c("primary", "secondary", "service", "residential",
                        "tertiary", "unclassified", "motorway", 
                        "motorway_link"))

streets_R_2 <- 
  streets_R |> 
  filter(highway %in% c("residential", "service", "unclassified"))

streets_R <- 
  streets_R |> 
  filter(highway %in% c("primary", "secondary", "tertiary", "motorway", 
                        "motorway_link"))

# Get daily reservations and prices
reservations_and_prices_R <- 
  daily_R |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

# Create monthly time series
monthly_series_R <- 
  reservations_and_prices_R |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) |> 
  relocate(price, .after = res)

# Create reservations model
reservations_model_R <- 
  monthly_series_R |> 
  filter(yearmon <= yearmonth("2020-01")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create reservations forecast
reservations_forecast_R <-
  reservations_model_R |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, res_trend_month = .mean)

# Create price model
price_model_R <- 
  monthly_series_R |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast_R <- 
  price_model_R |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_R <- 
  monthly_series_R |>  
  left_join(reservations_forecast_R, by = "yearmon") |> 
  left_join(price_forecast_R, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_R <-
  reservations_and_prices_R |> 
  mutate(prepan = date >= "2019-02-01" & date <= "2020-01-31") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series_R, -res, -price), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))

covid_res_dif_R <-
  reservations_and_prices_R |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total_R <-
  reservations_and_prices_R |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct_R <-
  {parse_number(covid_res_total_R) / (parse_number(covid_res_dif_R) + 
                                        parse_number(covid_res_total_R))} |> 
  scales::percent(0.1)

covid_price_pct_R <-
  reservations_and_prices_R |> 
  filter(date >= "2020-03-01") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)

# Get daily housing loss
housing_loss_daily_R <- 
  daily_R |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_daily_R <- 
  GH |> 
  filter(status != "B") |>
  st_filter(filter(CSD, name == "Richmond")) |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

housing_loss_daily_R <- 
  housing_loss_daily_R |> 
  left_join(GH_daily_R, by = "date") |> 
  mutate(units = FREH + GH) |>
  select(date, units)

commercial_pct_R <-
  daily_R |> 
  filter(date >= "2017-06-01", status != "B") |> 
  count(date) |> 
  left_join(housing_loss_daily_R, by = "date") |> 
  mutate(pct = units / n) |> 
  filter(date >= "2018-01-01") |> 
  summarize(mean = mean(pct, na.rm = TRUE)) |> 
  pull() |> 
  scales::percent(0.1)

# Create monthly time series
housing_loss_monthly_series_R <- 
  housing_loss_daily_R |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_R <- 
  housing_loss_monthly_series_R |> 
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast_R <-
  housing_loss_model_R |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_R <- 
  housing_loss_monthly_series_R |>  
  full_join(housing_loss_forecast_R, by = "yearmon")

# Add decay to growth rate
housing_loss_monthly_decay_R <-
  housing_loss_monthly_series_R |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into daily data
housing_loss_daily_model_R <-
  housing_loss_daily_R |> 
  add_row(date = as.Date(as.Date("2022-04-01", origin = "1970-01-01"):
                           as.Date("2023-12-31", origin = "1970-01-01"), 
                         origin = "1970-01-01")) |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_decay_R, -units), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

# Rent calculations
rent_str_2016_2021_R <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Richmond") |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

overpaid_2016_2021_R <- 
  rent_str_2016_2021_R |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rent_2019_R <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Richmond") |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019_R <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Richmond") |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019_R <- (rent_str_2019_R / rent_2019_R) |> scales::percent(0.1)

rent_change_table_raw_R <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Richmond") |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_R <- 
  rent_change_table_raw_R |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
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

str_incr_2017_2019_R <- 
  rent_change_table_R |> 
  filter(year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019_R <- 
  rent_change_table_R |> 
  filter(year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019_R <- 
  rent_change_table_R |> 
  filter(year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020_R <- 
  rent_change_table_R |> 
  filter(year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

# Rent projections
housing_loss_2023_R <- 
  housing_loss_daily_model_R |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023_R <-
  housing_loss_daily_model_R |> 
  filter((date == "2023-12-31" | date == "2021-12-31")) |> 
  summarize(dif = (units_trend[2] - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)

rent_inc_monthly_2021_2023_R <-
  rent_change_table_raw_R |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_R |> 
      filter((date == "2023-12-31" | date == "2021-12-31")) |> 
      group_by(tier = "CMA") |> 
      summarize(units = units[1], units_trend = units_trend[2], 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)

rent_inc_annual_2021_2023_R <- 
  (parse_number(rent_inc_monthly_2021_2023_R) * 12) |> 
  scales::dollar(1)

housing_loss_2023_raw_R <- 
  housing_loss_daily_model_R |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend)

rent_change_2023_table_R <- 
  rent_change_table_raw_R |> 
  filter(year == 5) |> 
  mutate(housing_loss = iv * renters / 100) |> 
  mutate(new_housing_loss = (housing_loss_2023_raw_R - sum(housing_loss)) * 
           (housing_loss / sum(housing_loss))) |> 
  mutate(str_2023 = new_housing_loss / renters * 100 * 
           model$coefficients[["iv"]],
         year_2023 = model$coefficients[["year"]]) |> 
  select(neighbourhood, total_rent, new_housing_loss, str_2023, year_2023) |> 
  mutate(rent_2023_pct = str_2023 / (total_rent + year_2023 * 2),
         rent_inc_pct = str_2023 / (str_2023 + year_2023 * 2))


# Nanaimo -----------------------------------------------------------------

property_N <- 
  property |> 
  filter(housing, city == "Nanaimo")

daily_N <- 
  daily |> 
  filter(housing, city == "Nanaimo")

active_avg_2021_N <- 
  daily_N |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2021_N <- 
  daily_N |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2021_N <- 
  daily_N |> 
  filter(year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021_N <- 
  daily_N |> 
  filter(year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021_N <- scales::dollar(rev_host_2021_N$avg, 100)
rev_med_2021_N <- scales::dollar(rev_host_2021_N$med, 100)

active_avg_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_N <- scales::dollar(rev_host_2019_N$avg, 100)
rev_med_2019_N <- scales::dollar(rev_host_2019_N$med, 100)

active_daily_N <- 
  daily |> 
  filter(housing, status %in% c("A", "R"), tier == "CA") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(group = if_else(name == "Nanaimo", "Nanaimo", 
                         "Mid-sized city average")) |> 
  group_by(date, group) |> 
  summarize(n_pct = sum(n, na.rm = TRUE) / sum(dwellings, na.rm = TRUE),
            .groups = "drop") |> 
  select(date, group, n_pct) |> 
  group_by(group) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-06-01", !is.na(group))

listings_pct_N <- 
  property_N |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_join(select(CT, GeoUID, dwellings = Dwellings)) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(pct = n() / mean(dwellings))

streets_N <- 
  (getbb("Nanaimo British Columbia") * c(1.01, 0.99, 0.99, 1.01)) |>  
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

streets_N <-
  rbind(
    streets_N$osm_polygons |>  st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets_N$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32610) |> 
  st_set_agr("constant") |> 
  st_intersection(filter(CSD, name == "Nanaimo"))

streets_N <-
  streets_N |>  
  select(osm_id, name, highway, geometry) |> 
  filter(highway %in% c("primary", "secondary", "service", "residential",
                        "tertiary", "unclassified", "motorway", 
                        "motorway_link"))

streets_N_2 <- 
  streets_N |> 
  filter(highway %in% c("residential", "service", "unclassified"))

streets_N <- 
  streets_N |> 
  filter(highway %in% c("primary", "secondary", "tertiary", "motorway", 
                        "motorway_link"))

# Get daily reservations and prices
reservations_and_prices_N <- 
  daily_N |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

# Create monthly time series
monthly_series_N <- 
  reservations_and_prices_N |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) |> 
  relocate(price, .after = res)

# Create reservations model
reservations_model_N <- 
  monthly_series_N |> 
  filter(yearmon <= yearmonth("2020-01")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create reservations forecast
reservations_forecast_N <-
  reservations_model_N |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, res_trend_month = .mean)

# Create price model
price_model_N <- 
  monthly_series_N |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast_N <- 
  price_model_N |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_N <- 
  monthly_series_N |>  
  left_join(reservations_forecast_N, by = "yearmon") |> 
  left_join(price_forecast_N, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_N <-
  reservations_and_prices_N |> 
  mutate(prepan = date >= "2019-02-01" & date <= "2020-01-31") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series_N, -res, -price), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))

covid_res_dif_N <-
  reservations_and_prices_N |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total_N <-
  reservations_and_prices_N |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct_N <-
  {parse_number(covid_res_total_N) / (parse_number(covid_res_dif_N) + 
                                      parse_number(covid_res_total_N))} |> 
  scales::percent(0.1)

covid_price_pct_N <-
  reservations_and_prices_N |> 
  filter(date >= "2020-03-01") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  scales::percent(0.1)

# Get daily housing loss
housing_loss_daily_N <- 
  daily_N |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_daily_N <- 
  GH |> 
  filter(status != "B") |>
  st_filter(filter(CSD, name == "Nanaimo")) |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

housing_loss_daily_N <- 
  housing_loss_daily_N |> 
  left_join(GH_daily_N, by = "date") |> 
  mutate(units = FREH + GH) |>
  select(date, units)

commercial_pct_N <-
  daily_N |> 
  filter(date >= "2017-06-01", status != "B") |> 
  count(date) |> 
  left_join(housing_loss_daily_N, by = "date") |> 
  mutate(pct = units / n) |> 
  filter(date >= "2018-01-01") |> 
  summarize(mean = mean(pct, na.rm = TRUE)) |> 
  pull() |> 
  scales::percent(0.1)
    
# Create monthly time series
housing_loss_monthly_series_N <- 
  housing_loss_daily_N |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_N <- 
  housing_loss_monthly_series_N |> 
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast_N <-
  housing_loss_model_N |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_N <- 
  housing_loss_monthly_series_N |>  
  full_join(housing_loss_forecast_N, by = "yearmon")

# Add decay to growth rate
housing_loss_monthly_decay_N <-
  housing_loss_monthly_series_N |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into daily data
housing_loss_daily_model_N <-
  housing_loss_daily_N |> 
  add_row(date = as.Date(as.Date("2022-04-01", origin = "1970-01-01"):
                           as.Date("2023-12-31", origin = "1970-01-01"), 
          origin = "1970-01-01")) |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_decay_N, -units), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

# Rent calculations
rent_str_2016_2021_N <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Nanaimo") |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

overpaid_2016_2021_N <- 
  rent_str_2016_2021_N |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rent_2019_N <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Nanaimo") |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019_N <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Nanaimo") |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019_N <- (rent_str_2019_N / rent_2019_N) |> scales::percent(0.1)

rent_change_table_raw_N <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Nanaimo") |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_N <- 
  rent_change_table_raw_N |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
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

str_incr_2017_2019_N <- 
  rent_change_table_N |> 
  filter(year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019_N <- 
  rent_change_table_N |> 
  filter(year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019_N <- 
  rent_change_table_N |> 
  filter(year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020_N <- 
  rent_change_table_N |> 
  filter(year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

# Rent projections
housing_loss_2023_N <- 
  housing_loss_daily_model_N |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023_N <-
  housing_loss_daily_model_N |> 
  filter((date == "2023-12-31" | date == "2021-12-31")) |> 
  summarize(dif = (units_trend[2] - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)

rent_inc_monthly_2021_2023_N <-
  rent_change_table_raw_N |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_N |> 
      filter((date == "2023-12-31" | date == "2021-12-31")) |> 
      group_by(tier = "CA") |> 
      summarize(units = units[1], units_trend = units_trend[2], 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)

rent_inc_annual_2021_2023_N <- 
  (parse_number(rent_inc_monthly_2021_2023_N) * 12) |> 
  scales::dollar(1)


# Kelowna -----------------------------------------------------------------

property_K <- 
  property |> 
  filter(housing, city == "Kelowna")

daily_K <- 
  daily |> 
  filter(housing, city == "Kelowna")

active_avg_2021_K <- 
  daily_K |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2021_K <- 
  daily_K |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2021_K <- 
  daily_K |> 
  filter(year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021_K <- 
  daily_K |> 
  filter(year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021_K <- scales::dollar(rev_host_2021_K$avg, 100)
rev_med_2021_K <- scales::dollar(rev_host_2021_K$med, 100)

active_avg_2019_K <- 
  daily_K |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2019_K <- 
  daily_K |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2019_K <- 
  daily_K |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2019_K <- 
  daily_K |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_K <- scales::dollar(rev_host_2019_K$avg, 100)
rev_med_2019_K <- scales::dollar(rev_host_2019_K$med, 100)

active_daily_K <- 
  daily |> 
  filter(housing, status %in% c("A", "R"), tier == "RES") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(group = if_else(name == "Kelowna", "Kelowna", 
                         "Resort community average")) |> 
  group_by(date, group) |> 
  summarize(n_pct = sum(n, na.rm = TRUE) / sum(dwellings, na.rm = TRUE),
            .groups = "drop") |> 
  select(date, group, n_pct) |> 
  group_by(group) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-06-01", !is.na(group))

listings_pct_K <- 
  property_K |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_join(select(CT, GeoUID, dwellings = Dwellings)) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(pct = n() / mean(dwellings))

streets_K <- 
  (getbb("Kelowna British Columbia") * c(1.01, 0.99, 0.99, 1.01)) |>  
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

streets_K <-
  rbind(
    streets_K$osm_polygons |>  st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets_K$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32610) |> 
  st_set_agr("constant") |> 
  st_intersection(filter(CSD, name == "Kelowna"))

streets_K <-
  streets_K |>  
  select(osm_id, name, highway, geometry) |> 
  filter(highway %in% c("primary", "secondary", "service", "residential",
                        "tertiary", "unclassified", "motorway", 
                        "motorway_link"))

streets_K_2 <- 
  streets_K |> 
  filter(highway %in% c("residential", "service", "unclassified"))

streets_K <- 
  streets_K |> 
  filter(highway %in% c("primary", "secondary", "tertiary", "motorway", 
                        "motorway_link"))

# Get daily reservations and prices
reservations_and_prices_K <- 
  daily_K |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

# Create monthly time series
monthly_series_K <- 
  reservations_and_prices_K |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) |> 
  relocate(price, .after = res)

# Create reservations model
reservations_model_K <- 
  monthly_series_K |> 
  filter(yearmon <= yearmonth("2020-01")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create reservations forecast
reservations_forecast_K <-
  reservations_model_K |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, res_trend_month = .mean)

# Create price model
price_model_K <- 
  monthly_series_K |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast_K <- 
  price_model_K |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_K <- 
  monthly_series_K |>  
  left_join(reservations_forecast_K, by = "yearmon") |> 
  left_join(price_forecast_K, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_K <-
  reservations_and_prices_K |> 
  mutate(prepan = date >= "2019-02-01" & date <= "2020-01-31") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series_K, -res, -price), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))

covid_res_dif_K <-
  reservations_and_prices_K |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total_K <-
  reservations_and_prices_K |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct_K <-
  {parse_number(covid_res_total_K) / (parse_number(covid_res_dif_K) + 
                                        parse_number(covid_res_total_K))} |> 
  scales::percent(0.1)

covid_price_pct_K <-
  reservations_and_prices_K |> 
  filter(date >= "2020-03-01") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)

covid_price_pct_2021_K <-
  reservations_and_prices_K |> 
  filter(date >= "2021-04-01", date <= "2021-09-30") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)


# Get daily housing loss
housing_loss_daily_K <- 
  daily_K |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_daily_K <- 
  GH |> 
  filter(status != "B") |>
  st_filter(filter(CSD, name == "Kelowna")) |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

housing_loss_daily_K <- 
  housing_loss_daily_K |> 
  left_join(GH_daily_K, by = "date") |> 
  mutate(units = FREH + GH) |>
  select(date, units)

commercial_pct_K <-
  daily_K |> 
  filter(date >= "2017-06-01", status != "B") |> 
  count(date) |> 
  left_join(housing_loss_daily_K, by = "date") |> 
  mutate(pct = units / n) |> 
  filter(date >= "2018-01-01") |> 
  summarize(mean = mean(pct, na.rm = TRUE)) |> 
  pull() |> 
  scales::percent(0.1)

# Create monthly time series
housing_loss_monthly_series_K <- 
  housing_loss_daily_K |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_K <- 
  housing_loss_monthly_series_K |> 
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast_K <-
  housing_loss_model_K |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_K <- 
  housing_loss_monthly_series_K |>  
  full_join(housing_loss_forecast_K, by = "yearmon")

# Add decay to growth rate
housing_loss_monthly_decay_K <-
  housing_loss_monthly_series_K |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into daily data
housing_loss_daily_model_K <-
  housing_loss_daily_K |> 
  add_row(date = as.Date(as.Date("2022-04-01", origin = "1970-01-01"):
                           as.Date("2023-12-31", origin = "1970-01-01"), 
                         origin = "1970-01-01")) |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_decay_K, -units), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

# Rent calculations
rent_str_2016_2021_K <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Central Okanagan J") |> 
  select(neighbourhood, FREH, total_rent, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

overpaid_2016_2021_K <- 
  rent_str_2016_2021_K |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rent_2019_K <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Central Okanagan J") |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019_K <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Central Okanagan J") |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019_K <- (rent_str_2019_K / rent_2019_K) |> scales::percent(0.1)

rent_change_table_raw_K <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Central Okanagan J") |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_K <- 
  rent_change_table_raw_K |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
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

str_incr_2017_2019_K <- 
  rent_change_table_K |> 
  filter(year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019_K <- 
  rent_change_table_K |> 
  filter(year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019_K <- 
  rent_change_table_K |> 
  filter(year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020_K <- 
  rent_change_table_K |> 
  filter(year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

# Rent projections
housing_loss_2023_K <- 
  housing_loss_daily_model_K |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023_K <-
  housing_loss_daily_model_K |> 
  filter((date == "2023-12-31" | date == "2021-12-31")) |> 
  summarize(dif = (units_trend[2] - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)

rent_inc_monthly_2021_2023_K <-
  rent_change_table_raw_K |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_K |> 
      filter((date == "2023-12-31" | date == "2021-12-31")) |> 
      group_by(tier = "RES/NU") |> 
      summarize(units = units[1], units_trend = units_trend[2], 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)

rent_inc_annual_2021_2023_K <- 
  (parse_number(rent_inc_monthly_2021_2023_K) * 12) |> 
  scales::dollar(1)


# Summerland --------------------------------------------------------------

property_S <- 
  property |> 
  filter(housing, city == "Summerland")

daily_S <- 
  daily |> 
  filter(housing, city == "Summerland")

active_avg_2021_S <- 
  daily_S |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2021_S <- 
  daily_S |> 
  filter(year(date) == 2021, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2021_S <- 
  daily_S |> 
  filter(year(date) == 2021, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2021_S <- 
  daily_S |> 
  filter(year(date) == 2021, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2021_S <- scales::dollar(rev_host_2021_S$avg, 100)
rev_med_2021_S <- scales::dollar(rev_host_2021_S$med, 100)

active_avg_2019_S <- 
  daily_S |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_avg_2019_S <- 
  daily_S |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

rev_total_2019_S <- 
  daily_S |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2019_S <- 
  daily_S |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_S <- scales::dollar(rev_host_2019_S$avg, 100)
rev_med_2019_S <- scales::dollar(rev_host_2019_S$med, 100)

active_daily_S <- 
  daily |> 
  filter(housing, status %in% c("A", "R"), tier == "NU") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(group = if_else(name == "Summerland", "Summerland", 
                         "Non-urban average")) |> 
  group_by(date, group) |> 
  summarize(n_pct = sum(n, na.rm = TRUE) / sum(dwellings, na.rm = TRUE),
            .groups = "drop") |> 
  select(date, group, n_pct) |> 
  group_by(group) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-06-01", !is.na(group))

listings_pct_S <- 
  property_S |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_join(select(CT, GeoUID, dwellings = Dwellings)) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(pct = n() / mean(dwellings))

streets_S <- 
  (getbb("Summerland British Columbia") * c(1.01, 0.99, 0.99, 1.01)) |>  
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

streets_S <-
  rbind(
    streets_S$osm_polygons |>  st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets_S$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32610) |> 
  st_set_agr("constant") |> 
  st_intersection(filter(CSD, name == "Summerland"))

streets_S <-
  streets_S |>  
  select(osm_id, name, highway, geometry) |> 
  filter(highway %in% c("primary", "secondary", "service", "residential",
                        "tertiary", "unclassified", "motorway", 
                        "motorway_link"))

streets_S_2 <- 
  streets_S |> 
  filter(highway %in% c("residential", "service", "unclassified"))

streets_S <- 
  streets_S |> 
  filter(highway %in% c("primary", "secondary", "tertiary", "motorway", 
                        "motorway_link"))

# Get daily reservations and prices
reservations_and_prices_S <- 
  daily_S |>  
  filter(housing, date >= "2017-06-01", status == "R") |> 
  group_by(date) |> 
  summarize(res = n(), price = mean(price), .groups = "drop")

# Create monthly time series
monthly_series_S <- 
  reservations_and_prices_S |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) |> 
  relocate(price, .after = res)

# Create reservations model
reservations_model_S <- 
  monthly_series_S |> 
  filter(yearmon <= yearmonth("2020-01")) |> 
  model(res = decomposition_model(
    STL(res, robust = TRUE), RW(season_adjust ~ drift())))

# Create reservations forecast
reservations_forecast_S <-
  reservations_model_S |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, res_trend_month = .mean)

# Create price model
price_model_S <- 
  monthly_series_S |> 
  filter(yearmon <= yearmonth("2019-12")) |> 
  model(price = decomposition_model(
    STL(price, robust = TRUE), RW(season_adjust ~ drift())))

# Create price forecast
price_forecast_S <- 
  price_model_S |> 
  forecast(h = "48 months") |> 
  as_tibble() |> 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_S <- 
  monthly_series_S |>  
  left_join(reservations_forecast_S, by = "yearmon") |> 
  left_join(price_forecast_S, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_S <-
  reservations_and_prices_S |> 
  mutate(prepan = date >= "2019-02-01" & date <= "2020-01-31") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(across(c(res, price), ~.x[prepan], .names = "{.col}_trend")) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(monthly_series_S, -res, -price), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:price_trend_month)) |> 
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                na.rm = TRUE, .before = 6)) |>
  mutate(across(c(res_trend, price_trend), 
                ~ifelse(date >= "2020-03-01", .x, NA)))

covid_res_dif_S <-
  reservations_and_prices_S |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

covid_res_total_S <-
  reservations_and_prices_S |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

covid_res_pct_S <-
  {parse_number(covid_res_total_S) / (parse_number(covid_res_dif_S) + 
                                        parse_number(covid_res_total_S))} |> 
  scales::percent(0.1)

covid_price_pct_S <-
  reservations_and_prices_S |> 
  filter(date >= "2020-03-01") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)

covid_price_pct_2021_S <-
  reservations_and_prices_S |> 
  filter(date >= "2021-04-01", date <= "2021-09-30") |> 
  summarize(dif = sum(price) / sum(price_trend) - 1) |> 
  pull(dif) |> 
  abs() |> 
  scales::percent(0.1)

# Get daily housing loss
housing_loss_daily_S <- 
  daily_S |>  
  filter(housing, date >= "2017-06-01") |> 
  group_by(date) |> 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_daily_S <- 
  GH |> 
  filter(status != "B") |>
  st_filter(filter(CSD, name == "Summerland")) |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarize(GH = sum(housing_units), .groups = "drop")

housing_loss_daily_S <- 
  housing_loss_daily_S |> 
  left_join(GH_daily_S, by = "date") |> 
  mutate(GH = coalesce(GH, 0)) |> 
  mutate(units = FREH + GH) |>
  select(date, units)

commercial_pct_S <-
  daily_S |> 
  filter(date >= "2017-06-01", status != "B") |> 
  count(date) |> 
  left_join(housing_loss_daily_S, by = "date") |> 
  mutate(pct = units / n) |> 
  filter(date >= "2018-01-01") |> 
  summarize(mean = mean(pct, na.rm = TRUE)) |> 
  pull() |> 
  scales::percent(0.1)

# Create monthly time series
housing_loss_monthly_series_S <- 
  housing_loss_daily_S |> 
  tsibble::as_tsibble(index = date) |> 
  tsibble::index_by(yearmon = yearmonth(date)) |> 
  summarize(units = mean(units))

# Create housing loss model
housing_loss_model_S <- 
  housing_loss_monthly_series_S |> 
  filter(yearmon <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast_S <-
  housing_loss_model_S |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series_S <- 
  housing_loss_monthly_series_S |>  
  full_join(housing_loss_forecast_S, by = "yearmon")

# Add decay to growth rate
housing_loss_monthly_decay_S <-
  housing_loss_monthly_series_S |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into daily data
housing_loss_daily_model_S <-
  housing_loss_daily_S |> 
  add_row(date = as.Date(as.Date("2022-04-01", origin = "1970-01-01"):
                           as.Date("2023-12-31", origin = "1970-01-01"), 
                         origin = "1970-01-01")) |> 
  mutate(prepan = date >= "2018-12-01" & date <= "2019-11-30") |> 
  mutate(date = if_else(date == "2020-02-29", as.Date("2020-02-28"), date)) |> 
  mutate(month = month(date), day = day(date)) |> 
  group_by(month, day) |> 
  mutate(units_trend = units[prepan]) |> 
  mutate(date = if_else(date == "2020-02-28", 
                        as.Date(c("2020-02-28", "2020-02-29", "2020-02-28", 
                                  "2020-02-29", "2020-02-28", "2020-02-29"))[
                                    seq_len(n())], date)) |> 
  ungroup() |> 
  mutate(yearmon = yearmonth(date)) |> 
  left_join(select(housing_loss_monthly_decay_S, -units), by = "yearmon") |> 
  group_by(yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

# Rent calculations
rent_str_2016_2021_S <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Summerland") |> 
  select(neighbourhood, FREH, total_rent, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

overpaid_2016_2021_S <- 
  rent_str_2016_2021_S |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rent_2019_S <- 
  cmhc$rent |> 
  filter(year == 2019) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Summerland") |> 
  select(neighbourhood, renters, total) |> 
  summarize(sum(total * renters * 12, na.rm = TRUE)) |>
  pull()

rent_str_2019_S <- 
  cmhc_str |> 
  filter(year + 2016 == 2019) |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |>
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Summerland") |> 
  filter(!is.na(tier)) |> 
  select(neighbourhood, FREH, iv, less_rent, renters) |> 
  summarize(sum(less_rent * renters * 12, na.rm = TRUE)) |> 
  pull()

rent_str_pct_2019_S <- (rent_str_2019_S / rent_2019_S) |> scales::percent(0.1)

rent_change_table_raw_S <-
  cmhc_str |> 
  mutate(less_rent = iv * model$coefficients[["iv"]]) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, name, renters), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$")) |> 
  filter(name == "Summerland") |> 
  select(neighbourhood, tier, year, total_rent, iv, less_rent, renters) |> 
  arrange(neighbourhood, year) |> 
  group_by(neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change) |> 
  ungroup()

rent_change_table_S <- 
  rent_change_table_raw_S |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
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

str_incr_2017_2019_S <- 
  rent_change_table_S |> 
  filter(year == "2017_2019") |> 
  pull(str_incr) |> 
  scales::percent(0.1)

rent_month_2017_2019_S <- 
  rent_change_table_S |> 
  filter(year == "2017_2019") |> 
  pull(mean_rent) |> 
  scales::dollar(01)

str_incr_month_2017_2019_S <- 
  rent_change_table_S |> 
  filter(year == "2017_2019") |> 
  pull(mean_str) |> 
  scales::dollar(01)

str_incr_2020_S <- 
  rent_change_table_S |> 
  filter(year == "2020") |> 
  pull(str_incr) |> 
  abs() |> 
  scales::percent(0.1)

# Rent projections
housing_loss_2023_S <- 
  housing_loss_daily_model_S |> 
  filter(date == "2023-12-31") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023_S <-
  housing_loss_daily_model_S |> 
  filter((date == "2023-12-31" | date == "2021-12-31")) |> 
  summarize(dif = (units_trend[2] - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)

rent_inc_monthly_2021_2023_S <-
  rent_change_table_raw_S |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_S |> 
      filter((date == "2023-12-31" | date == "2021-12-31")) |> 
      group_by(tier = "RES/NU") |> 
      summarize(units = units[1], units_trend = units_trend[2], 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)

rent_inc_annual_2021_2023_S <- 
  (parse_number(rent_inc_monthly_2021_2023_S) * 12) |> 
  scales::dollar(1)


# Save output -------------------------------------------------------------

qs::qsavem(property_V, active_avg_2021_V, active_avg_2019_V, hosts_avg_2021_V,
           hosts_avg_2019_V, rev_total_2021_V, rev_total_2019_V, rev_avg_2021_V,
           rev_avg_2019_V, rev_med_2021_V, rev_med_2019_V, active_daily_V,
           listings_pct_V, streets_V, streets_V_2, reservations_and_prices_V, 
           covid_res_pct_V, covid_price_pct_V, commercial_pct_V, 
           housing_loss_daily_model_V, overpaid_2016_2021_V, 
           rent_str_pct_2019_V, str_incr_2017_2019_V, rent_month_2017_2019_V, 
           str_incr_month_2017_2019_V, str_incr_2020_V, housing_loss_2023_V, 
           housing_loss_change_2021_2023_V, rent_inc_monthly_2021_2023_V, 
           rent_inc_annual_2021_2023_V, rent_change_2023_table_V,
           first_photo_pair, second_photo_pair, titles,
           
           property_R, active_avg_2021_R, active_avg_2019_R, hosts_avg_2021_R,
           hosts_avg_2019_R, rev_total_2021_R, rev_total_2019_R, rev_avg_2021_R,
           rev_avg_2019_R, rev_med_2021_R, rev_med_2019_R, active_daily_R,
           listings_pct_R, streets_R, streets_R_2, reservations_and_prices_R, 
           covid_res_pct_R, covid_price_pct_R, commercial_pct_R, 
           housing_loss_daily_model_R, overpaid_2016_2021_R, 
           rent_str_pct_2019_R, str_incr_2017_2019_R, rent_month_2017_2019_R, 
           str_incr_month_2017_2019_R, str_incr_2020_R, housing_loss_2023_R, 
           housing_loss_change_2021_2023_R, rent_inc_monthly_2021_2023_R, 
           rent_inc_annual_2021_2023_R, rent_change_2023_table_R,
           
           property_N, active_avg_2021_N, active_avg_2019_N, hosts_avg_2021_N,
           hosts_avg_2019_N, rev_total_2021_N, rev_total_2019_N, rev_avg_2021_N,
           rev_avg_2019_N, rev_med_2021_N, rev_med_2019_N, active_daily_N,
           listings_pct_N, streets_N, streets_N_2, reservations_and_prices_N, 
           covid_res_pct_N, covid_price_pct_N, commercial_pct_N, 
           housing_loss_daily_model_N, overpaid_2016_2021_N, 
           rent_str_pct_2019_N, str_incr_2017_2019_N, rent_month_2017_2019_N, 
           str_incr_month_2017_2019_N, str_incr_2020_N, housing_loss_2023_N, 
           housing_loss_change_2021_2023_N, rent_inc_monthly_2021_2023_N, 
           rent_inc_annual_2021_2023_N,

           property_K, active_avg_2021_K, active_avg_2019_K, hosts_avg_2021_K,
           hosts_avg_2019_K, rev_total_2021_K, rev_total_2019_K, rev_avg_2021_K,
           rev_avg_2019_K, rev_med_2021_K, rev_med_2019_K, active_daily_K,
           listings_pct_K, streets_K, streets_K_2, reservations_and_prices_K, 
           covid_res_pct_K, covid_price_pct_K, covid_price_pct_2021_K, 
           commercial_pct_K, housing_loss_daily_model_K, overpaid_2016_2021_K, 
           rent_str_pct_2019_K, str_incr_2017_2019_K, rent_month_2017_2019_K, 
           str_incr_month_2017_2019_K, str_incr_2020_K, housing_loss_2023_K, 
           housing_loss_change_2021_2023_K, rent_inc_monthly_2021_2023_K, 
           rent_inc_annual_2021_2023_K,

           property_S, active_avg_2021_S, active_avg_2019_S, hosts_avg_2021_S,
           hosts_avg_2019_S, rev_total_2021_S, rev_total_2019_S, rev_avg_2021_S,
           rev_avg_2019_S, rev_med_2021_S, rev_med_2019_S, active_daily_S,
           listings_pct_S, streets_S, streets_S_2, reservations_and_prices_S, 
           covid_res_pct_S, covid_price_pct_S, covid_price_pct_2021_S, 
           commercial_pct_S, housing_loss_daily_model_S, overpaid_2016_2021_S, 
           rent_str_pct_2019_S, str_incr_2017_2019_S, rent_month_2017_2019_S, 
           str_incr_month_2017_2019_S, str_incr_2020_S, housing_loss_2023_S, 
           housing_loss_change_2021_2023_S, rent_inc_monthly_2021_2023_S, 
           rent_inc_annual_2021_2023_S,
           
           file = "output/data/ch_3.qsm", nthreads = future::availableCores())
