#### Analysis for chapter 4 ####################################################

library(tidyverse)
library(lubridate)
library(slider)
library(tsibble)
library(feasts)
library(fable)
library(sf)

qs::qload("data/str_processed.qsm", nthreads = future::availableCores())
qs::qload("data/cma_comparison.qsm", nthreads = future::availableCores())
qs::qload("data/str_bc_processed.qsm", nthreads = future::availableCores())

col_palette <-
  c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
    "#AE7673")

property_vancouver <- property
daily_vancouver <- daily

rm(daily, daily_all, GH, property, daily_montreal_other, daily_toronto_other,
   property_montreal_other, property_toronto_other)

col_palette <-
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Vancouver trend analysis ------------------------------------------------

active_montreal <- 
  daily_montreal |> 
  filter(housing, status != "B") |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Montreal", type = "active")

active_toronto <- 
  daily_toronto |> 
  filter(housing, status != "B") |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Toronto", type = "active")

active_vancouver <- 
  daily_vancouver |> 
  filter(housing, status != "B") |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Vancouver", type = "active")

multi_montreal <- 
  daily_montreal |> 
  filter(housing, status != "B", multi) |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Montreal", type = "multi")

multi_toronto <- 
  daily_toronto |> 
  filter(housing, status != "B", multi) |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Toronto", type = "multi")

multi_vancouver <- 
  daily_vancouver |> 
  filter(housing, status != "B", multi) |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Vancouver", type = "multi")

FREH_montreal <- 
  daily_montreal |> 
  filter(housing) |> 
  group_by(date) |> 
  summarize(n = sum(FREH_3, na.rm = TRUE)) |> 
  mutate(n = slide_dbl(n, mean, .before = 20)) |> 
  mutate(city = "Montreal", type = "FREH")

FREH_toronto <- 
  daily_toronto |> 
  filter(housing) |> 
  group_by(date) |> 
  summarize(n = sum(FREH_3, na.rm = TRUE)) |> 
  mutate(n = slide_dbl(n, mean, .before = 20)) |> 
  mutate(city = "Toronto", type = "FREH")

FREH_vancouver <- 
  daily_vancouver |> 
  filter(housing) |> 
  group_by(date) |> 
  summarize(n = sum(FREH_3, na.rm = TRUE)) |> 
  mutate(n = slide_dbl(n, mean, .before = 20)) |> 
  mutate(city = "Vancouver", type = "FREH")

canada_comparison <- 
  bind_rows(active_montreal, active_toronto, active_vancouver,
          FREH_montreal, FREH_toronto, FREH_vancouver,
          multi_montreal, multi_toronto, multi_vancouver)

active_bc <- 
  daily_bc |> 
  filter(housing, status != "B") |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Rest of CMA", type = "active")

FREH_bc <- 
  daily_bc |> 
  filter(housing, status != "B") |> 
  group_by(date) |> 
  summarize(n = sum(FREH_3, na.rm = TRUE)) |> 
  mutate(n = slide_dbl(n, mean, .before = 20)) |> 
  mutate(city = "Rest of CMA", type = "FREH")

multi_bc <- 
  daily_bc |> 
  filter(housing, status != "B", multi) |> 
  count(date) |> 
  mutate(n = slide_dbl(n, mean, .before = 13)) |> 
  mutate(city = "Rest of CMA", type = "multi")

cma_comparison <- bind_rows(active_bc, active_vancouver, FREH_bc, 
                            FREH_vancouver, multi_bc, multi_vancouver)


# Figure 32 ---------------------------------------------------------------

fig_32 <- 
  canada_comparison |> 
  filter(date >= "2016-07-01", date <= "2019-12-31") |> 
  group_by(city, type) |> 
  mutate(index = 100 * n / n[date == "2017-01-01"]) |> 
  ungroup() |> 
  mutate(label = case_when(
    type == "active" & date == "2019-01-01" ~ city,
    TRUE ~ NA_character_)) |> 
  mutate(type = case_when(
    type == "active" ~ "Active listings", 
    type == "FREH" ~ "FREH listings",
    type == "multi" ~ "Multilistings")) |> 
  ggplot(aes(date, index, colour = city, size = city)) +
  geom_line() +
  geom_label(aes(date, index, label = label, colour = city), 
             fill = alpha("white", 0.75), size = 3) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-07-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(4, 6, 5)]) +
  scale_size_manual(values = c("Vancouver" = 1, "Montreal" = 0.5,
                               "Toronto" = 0.5), guide = "none") + 
  facet_wrap(vars(type), nrow = 1) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_32.png", fig_32, width = 9, height = 5)


# Figure 33 ---------------------------------------------------------------

fig_33 <- 
  cma_comparison |> 
  filter(date >= "2016-07-01", date <= "2019-12-31") |> 
  group_by(city, type) |> 
  mutate(index = 100 * n / n[date == "2017-01-01"]) |> 
  ungroup() |> 
  mutate(city = if_else(city == "Rest of CMA", "Rest of region", city)) |> 
  mutate(label = case_when(
    type == "active" & date == "2019-01-01" ~ city,
    TRUE ~ NA_character_)) |> 
  mutate(type = case_when(
    type == "active" ~ "Active listings", 
    type == "FREH" ~ "FREH listings",
    type == "multi" ~ "Multilistings")) |> 
  ggplot(aes(date, index, colour = city, size = city)) +
  geom_line() +
  geom_label(aes(date, index, label = label, colour = city), 
             fill = alpha("white", 0.75), size = 3) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-07-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(6, 5)]) +
  scale_size_manual(values = c("Vancouver" = 1, "Rest of region" = 0.5), 
                    guide = "none") + 
  facet_wrap(vars(type), nrow = 1) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_33.png", fig_33, width = 9, height = 5)


# Save output -------------------------------------------------------------

qs::qsavem(canada_comparison, cma_comparison, file = "output/data/ch_4.qsm")



