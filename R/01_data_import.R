#### BC DATA IMPORT ############################################################

library(tidyverse)
library(sf)
library(upgo)
library(strr)
library(cancensus)
library(future)
plan(multisession, workers = 10)

# upgo_connect()
# 
# property <- 
#   property_remote |> 
#   filter(country == "Canada", region == "British Columbia") |> 
#   collect()
# 
# daily <- 
#   daily_remote |> 
#   filter(property_ID %in% !!property$property_ID) |> 
#   collect()
# 
# host <- 
#   host_remote |> 
#   filter(host_ID %in% !!property$host_ID) |> 
#   collect()
# 
# daily <- strr_expand(daily)
# host <- strr_expand(host)
# 
# qs::qsavem(property, daily, host, file = "data/data.qsm", nthreads = 32)
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

housing_vectors <- c("v_CA16_4838", "v_CA16_4896", "v_CA16_6725")
housing_vectors_parent <- 
  cancensus::list_census_vectors("CA16") |> 
  filter(vector %in% housing_vectors) |> 
  pull(parent_vector)

DA <- get_census("CA16", regions = list(PR = "59"), level = "DA", 
                 geo_format = "sf", vectors = c("v_CA16_5750", "v_CA16_5753",
                                                "v_CA16_5699", housing_vectors,
                                                housing_vectors_parent)) |> 
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
