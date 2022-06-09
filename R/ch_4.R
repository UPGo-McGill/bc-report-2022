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


# Save output -------------------------------------------------------------

qs::qsavem(canada_comparison, cma_comparison, file = "output/data/ch_4.qsm")



