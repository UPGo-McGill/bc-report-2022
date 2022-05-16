#### BC DATA IMPORT ############################################################

library(tidyverse)
library(sf)
library(upgo)
library(strr)
library(cancensus)
library(future)
plan(multisession, workers = 10)

upgo_connect()

property <- 
  property_remote |> 
  filter(country == "Canada", region == "British Columbia") |> 
  collect()

daily <- 
  daily_remote |> 
  filter(property_ID %in% !!property$property_ID) |> 
  collect()

host <- 
  host_remote |> 
  filter(host_ID %in% !!property$host_ID) |> 
  collect()

daily <- strr_expand(daily)
host <- strr_expand(host)

qs::qsavem(property, daily, host, file = "data/data.qsm", nthreads = 32)
# qs::qload("data/data.qsm", nthreads = 32)


# Census data -------------------------------------------------------------

CT <- get_census("CA16", regions = list(PR = "59"), level = "CT", 
                 geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

CSD <- get_census("CA16", regions = list(PR = "59"), level = "CSD", 
                 geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

DA <- get_census("CA16", regions = list(PR = "59"), level = "DA", 
                 geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

CMA <- get_census("CA16", regions = list(PR = "59"), level = "CMA", 
                  geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

qs::qsavem(CT, CMA, CSD, DA, file = "data/geometry.qsm", 
           nthreads = availableCores())
