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

col_palette <-
  c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
    "#AE7673")


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

freh_2022 <- 
  daily |> 
  filter(housing, date == "2022-04-01") |> 
  summarize(FREH = sum(FREH_3)) |> 
  pull(FREH) |> 
  scales::comma(10)

gh_units_2022 <- 
  GH |> 
  st_drop_geometry() |> 
  filter(date == "2022-04-30", status != "B") |> 
  summarize(units = sum(housing_units)) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_2022 <- 
  housing_loss |> 
  filter(tier == "All", date == "2022-04-01") |> 
  pull(`Housing units`) |> 
  sum() |> 
  scales::comma(10)

housing_loss_change_pct_2022 <- 
  housing_loss |> 
  filter(tier == "All", year(date) %in% 2021:2022, month(date) == 4) |> 
  group_by(year = year(date)) |> 
  summarize(units = sum(`Housing units`)) |> 
  summarize(pct = (max(units) - min(units)) / min(units)) |> 
  pull() |> 
  scales::percent(0.1)

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


# Figure 7 ----------------------------------------------------------------

fig_7 <- 
  housing_loss |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  ggplot(aes(date, `Housing units`, fill = `Listing type`)) +
  geom_col(lwd = 0) +
  scale_fill_manual(values = col_palette[c(6, 5)]) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  facet_wrap(~tier, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_7.png", fig_7, width = 9, height = 5)


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
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(tier, yearmon, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  full_join(housing_loss_forecast, by = c("tier", "yearmon"))

# Add decay to growth rate
housing_loss_monthly_decay <-
  housing_loss_monthly_series |> 
  mutate(decay = 0.98 ^ (as.numeric(yearmon) - 602)) |> 
  group_by(tier) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[yearmon == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[yearmon == yearmonth("Mar 2020")] + 
      (lag * decay)) |> 
    ungroup()

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
  left_join(select(housing_loss_monthly_decay, -units), 
            by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  group_by(tier) |>
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  ungroup() |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

housing_loss_daily_model <- 
  housing_loss_daily_model |> 
  filter(tier != "All") |> 
  group_by(date) |> 
  summarize(units = sum(units, na.rm = TRUE), 
            units_trend = sum(units_trend, na.rm = TRUE)) |> 
  mutate(tier = "All", .before = date) |> 
  bind_rows(filter(housing_loss_daily_model, tier != "All")) |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

housing_loss_cc_end_2022 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2022 Apr")) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_cc_trend_end_2022 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2022 Apr")) |> 
  pull(units_trend_month) |> 
  scales::comma(10)

housing_loss_cc_dif_pct_end_2022 <- 
  housing_loss_monthly_decay |> 
  filter(tier == "CC", yearmon == yearmonth("2022 Apr")) |> 
  summarize(dif = units_trend_month / units - 1) |> 
  pull(dif) |> 
  scales::percent(0.1)


# Figure 8 ----------------------------------------------------------------

fig_8 <- 
  housing_loss_daily_model |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  group_by(tier) |>
  mutate(units_trend = slide_dbl(units_trend, mean, .before = 6, 
                                 .complete = TRUE)) |>
  ungroup() |>
  select(tier, date, units, units_trend) |> 
  pivot_longer(-c(tier, date)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    tier == "All" & date == "2020-07-05" & name == "units" ~ 
      "Actual housing loss", 
    tier == "All" & date == "2020-11-07" & name == "units_trend" ~ 
      "Expected housing loss",
    TRUE ~ NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = units, ymax = units_trend, group = 1),
              data = {
                housing_loss_daily_model |> 
                  mutate(tier = case_when(
                    tier == "All" ~ "All",
                    tier == "CA" ~ "Mid-sized cities",
                    tier == "CC" ~ "Core cities",
                    tier == "CMA" ~ "Large regions",
                    tier == "NU" ~ "Non-urban",
                    tier == "RES" ~ "Resort towns")) |> 
                  group_by(tier) |>
                  mutate(units_trend = slider::slide_dbl(
                    units_trend, mean, .before = 6, .complete = TRUE)) |>
                  ungroup()}, fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 0.5) +
  geom_label(aes(date, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tier, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_8.png", fig_8, width = 9, height = 5)




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

model_iv_coef_dollar <- scales::dollar(model$coefficients[["iv"]], 0.01)
model_year_coef_dollar <- scales::dollar(model$coefficients[["year"]], 0.01)
model_renter_coef_dollar <- scales::dollar(model$coefficients[["renter_pct"]], 
                                           0.01)

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


# Figure 9 ----------------------------------------------------------------

fig_9 <- 
  cmhc$rent |> 
  filter(!is.na(tier)) |> 
  mutate(tier = "All") |> 
  bind_rows(cmhc$rent) |> 
  select(tier, year, neighbourhood, total) |> 
  filter(!is.na(tier)) |> 
  mutate(tier = if_else(tier %in% c("NU", "RES"), "RES/NU", tier)) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "RES/NU" ~ "Resort towns / Non-urban")) |> 
  ggplot(aes(year, total, colour = tier)) +
  geom_line(aes(group = neighbourhood), alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4)], guide = "none") +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_9.png", fig_9, width = 9, height = 5)


# Figure 10 ---------------------------------------------------------------

find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- (q3 - q1) * 1.5
  which(x < q1 - iqr | x > q3 + iqr)
}

fig_10 <-
  cmhc_str[-find_outliers(cmhc_str$iv), ] |> 
  mutate(tier = "All") |> 
  bind_rows(cmhc_str[-find_outliers(cmhc_str$iv), ]) |> 
  mutate(year = year + 2016) |> 
  filter(!is.na(tier)) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "RES/NU" ~ "Resort towns / Non-urban")) |> 
  ggplot(aes(iv, total_rent)) +
  geom_point(aes(color = year), size = 0.5) +
  geom_smooth(color = "black", se = FALSE, method = "lm") +
  scale_y_continuous(name = "Average monthly rent", labels = scales::dollar) +
  scale_x_continuous(name = "Dedicated STRs per 100 rental units") +
  scale_color_stepsn(name = NULL, colours = col_palette[c(1, 2, 5)]) +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(legend.position = c(0.97, 0.12), legend.justification = c(1, 0),
        legend.key.width = unit(1, "cm"), legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_10.png", fig_10, width = 9, height = 5)


# Figure 11 ---------------------------------------------------------------

fig_11 <- 
  rent_change_table_year |> 
  select(year, tier, med_rent, raw_rent) |> 
  pivot_longer(med_rent:raw_rent) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "RES/NU" ~ "Resort towns / Non-urban")) |> 
  arrange(year, tier, desc(name)) |> 
  ggplot() +
  geom_col(aes(year, value, group = name, fill = name),
           position = position_dodge(width = -0.5)) +
  geom_segment(data = rent_change_table_year |> 
                 mutate(tier = case_when(
                   tier == "All" ~ "All",
                   tier == "CA" ~ "Mid-sized cities",
                   tier == "CC" ~ "Core cities",
                   tier == "CMA" ~ "Large regions",
                   tier == "RES/NU" ~ "Resort towns / Non-urban")),
               mapping = aes(x = year, xend = year, y = raw_rent, 
                             yend = med_rent),
               size = 1,
               arrow = arrow(length = unit(0.3, "cm")),
               position = position_nudge(x = -0.125)) +
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_x_continuous((name = NULL)) +
  scale_fill_brewer(name = NULL, palette = "Accent", labels = c(
    "Actual median rent change",
    "Estimated median rent change\nwith no change in dedicated STRs")) +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(legend.position = c(0.97, 0.12), legend.justification = c(1, 0),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_11.png", fig_11, width = 9, height = 5)


# Table 3 -----------------------------------------------------------------

rent_change_table |> 
  select(-starts_with("mean")) |> 
  pivot_wider(names_from = year, values_from = med_rent:str_incr) |> 
  relocate(ends_with("2019"), .after = tier) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "RES/NU" ~ "Resort towns / Non-urban")) |> 
  mutate(across(c(med_rent_2017_2019, med_str_2017_2019, med_rent_2020,
                  med_str_2020), scales::dollar, 1),
         across(contains("incr"), scales::percent, 0.1)) |> 
  mutate(med_str_2017_2019 = paste0(med_str_2017_2019, " (", med_incr_2017_2019, ")"),
         med_str_2020 = paste0(med_str_2020, " (", med_incr_2020, ")")) |> 
  select(-med_incr_2017_2019, -med_incr_2020) |> 
  set_names(c("Community type", 
              "Median YOY monthly rent chg. (2017-19)",
              "Median YOY impact of STR chg. on monthly rent chg. (2017-19)",
              "Total YOY impact of STR chg. on monthly rent chg. (2017-19)",
              "Median YOY monthly rent chg. (2020)",
              "Median YOY impact of STR chg. on monthly rent chg. (2020)",
              "Total YOY impact of STR chg. on monthly rent chg. (2020)")) |> 
  gt::gt()


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

# Integrate forecast into daily data
housing_loss_daily_model_2023 <-
  expand.grid(unique(housing_loss_daily_model$tier), 
            as.Date("2022-05-01"):as.Date("2023-12-31")) |> 
  set_names(c("tier", "date")) |> 
  as_tibble() |> 
  mutate(date = as.Date(date, origin = "1970-01-01")) |> 
  arrange(tier, date) |> 
  bind_rows(housing_loss_daily_model, x = _) |> 
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
  left_join(select(housing_loss_monthly_decay, -units), 
            by = c("tier", "yearmon")) |> 
  group_by(tier, yearmon) |> 
  mutate(units_trend = units_trend * units_trend_month / mean(units_trend)) |> 
  ungroup() |> 
  select(-c(prepan:day, yearmon:units_trend_month, decay, lag)) |> 
  group_by(tier) |>
  mutate(units_trend = slider::slide_dbl(units_trend, mean, na.rm = TRUE, 
                                         .before = 6)) |> 
  ungroup() |> 
  mutate(units_trend = if_else(date >= "2020-03-01", units_trend, NA_real_))

housing_loss_2023 <- 
  housing_loss_daily_model_2023 |> 
  filter(date == "2023-12-31", tier == "All") |> 
  pull(units_trend) |> 
  scales::comma(100)

housing_loss_change_2021_2023 <-
  housing_loss_daily_model_2023 |> 
  filter((date == "2023-12-31" | date == "2019-12-31"), tier == "All") |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) / 
              sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)
  
rent_inc_monthly_2021_2023 <-
  rent_change_table_raw |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_2023 |> 
      filter((date == "2023-12-31" | date == "2019-12-31")) |> 
      mutate(tier = if_else(tier %in% c("NU", "RES"), "RES/NU", tier)) |> 
      group_by(tier, date) |> 
      summarize(units = sum(units), units_trend = sum(units_trend), 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE)) /
              mean(renters)) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100) |> 
  filter(tier == "All") |> 
  pull(rent_inc) |> 
  scales::dollar(0.01)
  
rent_inc_annual_2021_2023 <- 
  (parse_number(rent_inc_monthly_2021_2023) * 12) |> 
  scales::dollar(1)

rent_total_2021_2023 <-
  rent_change_table_raw |> 
  filter(year == 5) |> 
  group_by(tier) |> 
  summarize(renters = sum(renters)) |> 
  left_join(
    housing_loss_daily_model_2023 |> 
      filter((date == "2023-12-31" | date == "2019-12-31")) |> 
      mutate(tier = if_else(tier %in% c("NU", "RES"), "RES/NU", tier)) |> 
      group_by(tier, date) |> 
      summarize(units = sum(units), units_trend = sum(units_trend), 
                .groups = "drop")) |> 
  group_by(tier) |> 
  summarize(dif = (sum(units_trend, na.rm = TRUE) - sum(units, na.rm = TRUE))) |> 
  mutate(rent_inc = dif * model$coefficients[["iv"]] * 100 * 12) |> 
  filter(tier == "All") |> 
  pull(rent_inc) |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")


# Figure 12 ---------------------------------------------------------------

fig_12 <- 
  housing_loss_daily_model_2023 |> 
  group_by(tier) |>
  mutate(units_trend = slide_dbl(units_trend, mean, .before = 6, 
                                 .complete = TRUE)) |>
  ungroup() |>
  select(tier, date, units, units_trend) |> 
  pivot_longer(-c(tier, date)) |> 
  filter(!is.na(value)) |> 
  mutate(tier = case_when(
    tier == "All" ~ "All",
    tier == "CA" ~ "Mid-sized cities",
    tier == "CC" ~ "Core cities",
    tier == "CMA" ~ "Large regions",
    tier == "NU" ~ "Non-urban",
    tier == "RES" ~ "Resort towns")) |> 
  mutate(label = case_when(
    tier == "All" & date == "2020-06-05" & name == "units" ~ 
      "Actual housing loss", 
    tier == "All" & date == "2021-04-07" & name == "units_trend" ~ 
      "Expected housing loss",
    TRUE ~ NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = units, ymax = units_trend, group = 1),
              data = {
                housing_loss_daily_model_2023 |> 
                  mutate(tier = case_when(
                    tier == "All" ~ "All",
                    tier == "CA" ~ "Mid-sized cities",
                    tier == "CC" ~ "Core cities",
                    tier == "CMA" ~ "Large regions",
                    tier == "NU" ~ "Non-urban",
                    tier == "RES" ~ "Resort towns")) |> 
                  group_by(tier) |>
                  mutate(units_trend = slider::slide_dbl(
                    units_trend, mean, .before = 6, .complete = TRUE)) |>
                  ungroup()}, fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1) +
  geom_label(aes(date, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_date(name = NULL, limits = as.Date(c("2018-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tier, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_12.png", fig_12, width = 9, height = 5)

  
# Save output -------------------------------------------------------------

qs::qsavem(housing_loss, freh_2022, gh_units_2022, housing_loss_2022, 
           housing_loss_change_pct_2022, housing_loss_2021,
           active_decline_pct_2019_2021, housing_loss_2019,
           housing_loss_decline_pct_2019_2021, housing_loss_daily_model,
           housing_loss_cc_end_2022, housing_loss_cc_trend_end_2022,
           housing_loss_cc_dif_pct_end_2022, rent_tier, cmhc, cmhc_str,
           model, model_iv_coef_dollar, model_year_coef_dollar, 
           model_renter_coef_dollar, rent_2016_2021_dollar,
           rent_str_pct_2016_2021, overpaid_2016_2021, rent_str_pct_2019,
           str_incr_2017_2019, rent_month_2017_2019, str_incr_month_2017_2019,
           str_incr_2020, rent_month_2020, str_incr_month_2020,
           rent_change_table, rent_change_table_year,
           housing_loss_daily_model_2023, housing_loss_2023,
           housing_loss_change_2021_2023, rent_inc_monthly_2021_2023,
           rent_inc_annual_2021_2023, rent_total_2021_2023,
           file = "output/data/ch_2.qsm", nthreads = future::availableCores())
