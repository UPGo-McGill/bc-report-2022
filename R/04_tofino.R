#### Tofino analysis #########

library(tsibble)
library(feasts)
library(fable)
library(lubridate)

col_palette <-
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# Get daily reservations and prices
reservations_and_prices_tf <- 
  daily %>% 
  filter(housing, date >= "2017-06-01", status == "R", city == "Tofino") %>% 
  group_by(date) %>% 
  summarize(res = n(), price = mean(price))

# Create monthly time series
monthly_series_tf <- 
  reservations_and_prices_tf %>% 
  tsibble::as_tsibble(index = date) %>% 
  tsibble::index_by(yearmon = yearmonth(date)) %>% 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) %>% 
  relocate(price, .after = res)

# Create reservations model
reservations_model_tf <- 
  monthly_series_tf %>%
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(res = decomposition_model(
    STL(res, robust = TRUE), NAIVE(season_adjust)))

# Create price model
price_model_tf <- 
  monthly_series_tf %>% 
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(price = decomposition_model(
    STL(price, robust = TRUE), NAIVE(season_adjust)))

# Create reservations forecast
reservations_forecast_tf <-
  reservations_model_tf %>% 
  forecast(h = "24 months") %>% 
  as_tibble() %>% 
  select(yearmon, res_trend_month = .mean)

# Create price forecast
price_forecast_tf <- 
  price_model_tf %>% 
  forecast(h = "24 months") %>% 
  as_tibble() %>% 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series_tf <- 
  monthly_series_tf %>% 
  left_join(reservations_forecast_tf, by = "yearmon") %>% 
  left_join(price_forecast_tf, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices_tf <- 
  reservations_and_prices_tf %>% 
  mutate(across(c(res, price), slider::slide_dbl, ~.x[1], .before = 366, 
                .complete = TRUE, .names = "{.col}_trend")) %>%
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                .before = 6, .complete = TRUE)) %>%
  mutate(across(c(res_trend, price_trend), 
                ~if_else(date >= "2020-03-01", .x, NA_real_))) %>%
  mutate(yearmon = yearmonth(date)) %>%
  left_join(select(monthly_series_tf, -res, -price), by = "yearmon") %>%
  group_by(yearmon) %>%
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) %>%
  ungroup() %>%
  select(-c(yearmon:price_trend_month))

res_total_pandemic_dif_tf <-
  reservations_and_prices_tf |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_dif = sum(res_trend - res)) |> 
  pull(res_dif) |> 
  scales::comma(100)

res_total_pandemic_tf <-
  reservations_and_prices_tf |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res)) |> 
  pull(res_tot) |> 
  scales::comma(100)

res_total_pandemic_pct_tf <-
  {(parse_number(res_total_pandemic_tf) - parse_number(res_total_pandemic_dif_tf)) / 
      parse_number(res_total_pandemic_tf)} %>%
  scales::percent(0.1)

res_total_pandemic_trend_tf <-
  reservations_and_prices_tf |> 
  filter(date >= "2020-03-01") |> 
  summarize(res_tot = sum(res_trend)) |> 
  pull(res_tot) |> 
  scales::comma(100)

price_difs_tf <- 
  reservations_and_prices_tf |> 
  mutate(price_dif = price - price_trend,
         price_dif_pct = (price - price_trend) / price_trend) |> 
  summarize(max_price = max(price, na.rm = TRUE),
            max_dif = max(price_dif, na.rm = TRUE),
            max_dif_pct = max(price_dif_pct, na.rm = TRUE),
            max_price_date = date[which.max(price)],
            max_dif_date = date[which.max(price_dif)],
            max_dif_pct_date = date[which.max(price_dif_pct)])

max_price_tf <- scales::dollar(price_difs_tf$max_price, 1)

var_rise_price_tf <-
  daily |> 
  filter(housing, city == "Tofino", year(date) %in% c(2019, 2021),
         month(date) <= month(max(daily$date))) |> 
  group_by(year_2021 = year(date) == 2021) |> 
  summarize(avg_price = mean(price)) |> 
  summarize((avg_price[2] - avg_price[1]) / avg_price[1]) |> 
  pull() |> 
  scales::percent(0.1)

tofino_res_graph <- 
  reservations_and_prices_tf %>% 
    mutate(res = slider::slide_dbl(res, mean, .before = 6, .complete = TRUE)) %>% 
    select(date, res, res_trend) %>% 
    pivot_longer(-date) %>% 
    filter(!is.na(value)) %>%
    mutate(label = case_when(
      date == "2019-07-05" & name == "res" ~ "Actual reservations", 
      date == "2020-08-05" & name == "res_trend" ~ "Expected reservations",
      TRUE ~ NA_character_)) %>%
    ggplot() +
    geom_ribbon(aes(x = date, ymin = res, ymax = res_trend, group = 1),
                data = {reservations_and_prices_tf %>% 
                    mutate(res = slider::slide_dbl(res, mean, .before = 6, .complete = TRUE))
                }, fill = col_palette[3],
                alpha = 0.3) +
    geom_line(aes(date, value, color = name), lwd = 1) +
    geom_label(aes(date, value, label = label, color = name), size = 3) +
    scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
    scale_y_continuous(name = NULL, limits = c(0, NA), 
                       label = scales::comma) +
    scale_color_manual(name = NULL, 
                       labels = c("Actual reservations", "Expected reservations"), 
                       values = col_palette[c(5, 1)]) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          text = element_text(face = "plain", family = "Futura"),
          plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/tofino_res_graph.png", plot = tofino_res_graph, width = 9, 
       height = 6)

tofino_price_graph <- 
  reservations_and_prices_tf %>% 
  mutate(price = slider::slide_dbl(price, mean, .before = 6, .complete = TRUE)) %>% 
  select(date, price, price_trend) %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>%
  mutate(label = case_when(
    date == "2019-07-05" & name == "price" ~ "Actual nightly price", 
    date == "2021-04-15" & name == "price_trend" ~ "Expected nightly price",
    TRUE ~ NA_character_)) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = price, ymax = price_trend, group = 1),
              data = {reservations_and_prices_tf %>% 
                  mutate(price = slider::slide_dbl(price, mean, .before = 6, .complete = TRUE))
                }, fill = col_palette[3],
              alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1) +
    geom_label(aes(date, value, label = label, color = name), size = 3) +
  scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(150, NA), 
                     label = scales::dollar) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/tofino_price_graph.png", plot = tofino_price_graph, width = 9, 
       height = 6)
