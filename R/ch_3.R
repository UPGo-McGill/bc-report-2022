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



CSD |> 
  st_drop_geometry() |> 
  select(name, population, tier) |> 
  group_by(tier) |> 
  arrange(-population) |> 
  slice(1:3)


case_studies <- c("Vancouver", "Richmond", "Nanaimo", "Kelowna", "Summerland")




# Vancouver ---------------------------------------------------------------







# Richmond ----------------------------------------------------------------





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

active_change_pct_N <- 
  ((parse_number(active_avg_2019_N) - parse_number(active_avg_2021_N)) / 
     parse_number(active_avg_2019_N)) |> 
  scales::percent(0.1)

hosts_avg_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status %in% c("A", "R")) |> 
  count(date, host_ID) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(1)

hosts_change_pct_N <- 
  ((parse_number(hosts_avg_2019_N) - parse_number(hosts_avg_2021_N)) / 
     parse_number(hosts_avg_2019_N)) |> 
  scales::percent(0.1)

rev_total_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status == "R") |> 
  pull(price) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_change_pct_N <- 
  ((parse_number(rev_total_2019_N) - parse_number(rev_total_2021_N)) / 
     parse_number(rev_total_2019_N)) |> 
  scales::percent(0.1)

rev_host_2019_N <- 
  daily_N |> 
  filter(year(date) == 2019, status == "R", !is.na(host_ID)) |> 
  group_by(host_ID) |> 
  summarize(rev = sum(price)) |> 
  summarize(avg = mean(rev), 
            med = median(rev))

rev_avg_2019_N <- scales::dollar(rev_host_2019_N$avg, 100)
rev_med_2019_N <- scales::dollar(rev_host_2019_N$med, 100)

rev_avg_change_pct_N <- ((rev_host_2019_N$avg - rev_host_2021_N$avg) / 
                         rev_host_2019_N$avg) |> 
  scales::percent(0.1)

rev_med_change_pct_N <- ((rev_host_2019_N$med - rev_host_2021_N$med) / 
                         rev_host_2019_N$med) |> 
  scales::percent(0.1)

active_daily_N <- 
  daily |> 
  filter(status %in% c("A", "R"), tier == "CA") |> 
  count(CSDUID, date) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = GeoUID, name, dwellings),
            by = "CSDUID") |> 
  mutate(n_pct = n / dwellings) |> 
  select(name, date, n_pct) |> 
  group_by(name) |> 
  mutate(n_pct = slide_dbl(n_pct, mean, na.rm = TRUE, .before = 6)) |> 
  ungroup() |> 
  filter(!is.na(name), date >= "2017-06-01") |> 
  mutate(Nanaimo = if_else(name == "Nanaimo", "Nanaimo", NA_character_))

active_daily_N <- 
  active_daily_N |> 
  group_by(date) |> 
  summarize(n_pct = mean(n_pct),
            Nanaimo = "Mid-sized city average") |> 
  bind_rows(active_daily_N)

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
  filter(highway %in% c("primary", "secondary", "service", "residential"))

streets_N_2 <- 
  streets_N |> 
  filter(highway %in% c("residential", "service"))

streets_N <- 
  streets_N |> 
  filter(highway %in% c("primary", "secondary"))

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
            res = sum(res)) %>% 
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

housing_loss_cc_end_2021 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_cc_trend_end_2021 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  pull(units_trend_month) |> 
  scales::comma(10)

housing_loss_cc_dif_pct_end_2021 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2021 Dec")) |> 
  summarize(dif = units_trend_month / units - 1) |> 
  pull(dif) |> 
  scales::percent(0.1)




# Kelowna -----------------------------------------------------------------





# Summerland --------------------------------------------------------------



# Save output -------------------------------------------------------------

qs::qsavem(property_N, active_avg_2021_N, active_avg_2019_N, hosts_avg_2021_N,
           hosts_avg_2019_N, rev_total_2021_N, rev_total_2019_N, rev_avg_2021_N,
           rev_avg_2019_N, rev_med_2021_N, rev_med_2019_N, active_daily_N,
           streets_N, streets_N_2, reservations_and_prices_N, covid_res_pct_N,
           covid_price_pct_N, housing_loss_daily_model_N,
           file = "output/data/ch_3.qsm", nthreads = future::availableCores())


