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


# STR-induced housing loss ------------------------------------------------

FREH_total <- 
  daily |> 
  filter(housing, date >= "2016-01-01") |> 
  group_by(tier, date) |>  
  summarize(FREH = sum(FREH_3), .groups = "drop") |> 
  filter(day(date) == 1)

GH_total <-
  GH |> 
  st_drop_geometry() |> 
  group_by(tier, date) |> 
  summarize(GH = sum(housing_units), .groups = "drop") |> 
  mutate(GH = slide_dbl(GH, mean, .before = 29))

housing_loss <-
  FREH_total |> 
  select(date, FREH) |>  
  left_join(GH_total, by = "date") |> 
  rename(`Entire home/apt` = FREH, `Private room` = GH) |> 
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") |> 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  




# Save output -------------------------------------------------------------

qs::qsavem(housing_loss,
           file = "output/data/ch_2.qsm", nthreads = future::availableCores())


