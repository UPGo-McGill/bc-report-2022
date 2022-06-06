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

daily <- strr_expand(daily)

host <-
  host_remote |>
  filter(host_ID %in% !!property$host_ID) |>
  collect()

host <- strr_expand(host)

qs::qsavem(property, daily, host, file = "output/data/data.qsm", nthreads = 32)
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
  st_as_sf() |> 
  rename(arts = `v_CA16_5750: 71 Arts, entertainment and recreation`,
         accomodation = `v_CA16_5753: 72 Accommodation and food services`,
         all_industry = `v_CA16_5699: All industry categories`)

CMA <- get_census("CA16", regions = list(PR = "59"), level = "CMA",
                  geo_format = "sf") |>
  st_transform(32610) |>
  as_tibble() |>
  st_as_sf()

# CMA from cancensus is broken. Import the one from the web census
province <- cancensus::get_census("CA16", regions = list(PR = "59"), 
                                  level = "PR", geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

CMA_shapefile <- 
  read_sf("data/shapefiles/lcma000b16a_e.shp") |> 
  st_transform(32610) |>
  st_filter(province)

CMA_shapefile <- 
  CMA_shapefile |> 
  transmute(GeoUID = CMAPUID,
            CMANAME,
            type = case_when(CMATYPE == "B" ~ "CMA",
                             CMATYPE == "D" ~ "CA",
                             CMATYPE == "K" ~ "CA"))

CMA <- 
  CMA |> 
  st_drop_geometry() |> 
  left_join(CMA_shapefile, by = "GeoUID") |> 
  select(-Type) |> 
  st_as_sf() |> 
  st_transform(32610)

# A lot of NAs from the employment vectors at the CMA level. Get them from DA
CMA_tourism_industry <- 
  DA |> 
  st_drop_geometry() |> 
  group_by(CMA_UID) |> 
  summarize(arts = sum(arts, na.rm = TRUE),
            accomodation = sum(accomodation, na.rm = TRUE),
            all_industry = sum(all_industry, na.rm = TRUE),
            .groups = "keep") |> 
  summarize(tourism = (arts + accomodation) / all_industry,
            .groups = "drop")

CMA <- 
  CMA |> 
  left_join(CMA_tourism_industry, by = c("GeoUID" = "CMA_UID")) |> 
  mutate(tier = if_else(tourism >= 0.12, "RES", type)) |> 
  select(-type)

CSD <- get_census("CA16", regions = list(PR = "59"), level = "CSD", 
                  geo_format = "sf", vectors = c("v_CA16_5750", "v_CA16_5753",
                                                 "v_CA16_5699")) |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(GeoUID,
         households = Households,
         dwellings = Dwellings,
         population = Population,
         name,
         CMA_UID,
         arts = `v_CA16_5750: 71 Arts, entertainment and recreation`,
         accomodation = `v_CA16_5753: 72 Accommodation and food services`,
         all_industry = `v_CA16_5699: All industry categories`)

CSD <- 
  CSD |> 
  # Add CMA tiers already calculated
  left_join(select(st_drop_geometry(CMA), GeoUID, tier), 
            by = c("CMA_UID" = "GeoUID")) |> 
  # Add central cities
  mutate(tier = if_else(name %in% c("Vancouver (CY)", "Abbotsford (CY)",
                                    "Victoria (CY)"), "CC", tier)) |> 
  # Add RES to the RES to CSDs
  mutate(tier = if_else(is.na(tier) &
                          (arts + accomodation) / all_industry >= 0.12,
                        "RES", tier)) |> 
  # The rest is non urban
  mutate(tier = if_else(is.na(tier), "NU", tier))

CSD <- 
  CSD |> 
  mutate(name = str_remove(name, " \\([^\\(]*\\)$"))


# Save --------------------------------------------------------------------

qs::qsavem(CT, CMA, CSD, DA, CSD, province, file = "output/data/geometry.qsm", 
           nthreads = future::availableCores())
