#### DATA PROCESSING ###########################################################

library(tidyverse)
library(strr)
library(sf)
library(future)
plan(multisession)

qs::qload("data/data.qsm", nthreads = availableCores())


# Cities to look at -------------------------------------------------------

top_tier <- c("Vancouver", "Victoria", "Whistler", "Richmond", "Burnaby",
              "Surrey", "North Vancouver", "Saanich", "Coquitlam",
              "West Vancouver")

mid_tier <- c("Kelowna", "Penticton", "Vernon", "Nanaimo", "Cranbrook")

resort <- c("Parksville", "Qualicum Beach", "Courtenay", "Tofino", "Ucluelet", 
            "Osoyoos", "Oliver", "Revelstoke", "Golden", "Valemount", 
            "Clearwater", "Smithers", "Invermere", "Radium Hot Springs", 
            "Nelson", "Fernie", "Squamish")

resource <- c("Prince George", "Fort St. John", "Prince Rupert", "Kamloops")


# Convert to CAD ----------------------------------------------------------

exchange_rates <- upgo::convert_currency(start_date = min(daily$date), 
                                         end_date = max(daily$date))

daily <-
  daily  |>
  mutate(year_month = substr(date, 1, 7)) |> 
  left_join(exchange_rates) |> 
  mutate(price = price * exchange_rate) |> 
  select(-year_month, -exchange_rate)


# Calculate ML ------------------------------------------------------------

daily <- strr_multi(daily, host)


# Calculate FREH and GH ---------------------------------------------------

FREH <- strr_FREH(daily)
GH <- strr_ghost(property)


# Add region classification to property/daily -----------------------------

to_remove <- c("city", "reg_type", "CSDUID", "CSDUID.x", "CSDUID.y", 
               "CSDUID.x.x", "CSDUID.y.y", "CSDUID.x.x.x", "CSDUID.y.y.y")

prop_city <- 
  property |> 
  strr_as_sf(32610) |> 
  select(property_ID) |> 
  st_intersection(CSD) |> 
  st_drop_geometry() |> 
  select(property_ID, city = name, CSDUID = GeoUID) |> 
  mutate(city = str_remove(city, " \\(.*\\)$"))

property <- 
  property |> 
  select(-any_of(to_remove)) |> 
  left_join(prop_city, by = "property_ID") |> 
  mutate(reg_type = case_when(
    city %in% top_tier ~ "Top-tier",
    city %in% mid_tier ~ "Mid-tier",
    city %in% resource ~ "Resource",
    city %in% resort ~ "Resort",
    TRUE ~ "Other"))

daily <- 
  daily |> 
  select(-any_of(to_remove)) |> 
  left_join(prop_city, by = "property_ID") |> 
  mutate(reg_type = case_when(
    city %in% top_tier ~ "Top-tier",
    city %in% mid_tier ~ "Mid-tier",
    city %in% resource ~ "Resource",
    city %in% resort ~ "Resort",
    TRUE ~ "Other"))


# Add DA ------------------------------------------------------------------

DA_to_join <- 
  property |> 
  strr_as_sf(32610) |> 
  select(property_ID) |> 
  st_intersection(DA) |> 
  st_drop_geometry() |> 
  select(property_ID, DAUID = GeoUID)

property <- 
  property |>
  left_join(DA_to_join)

daily <- 
  daily |> 
  left_join(DA_to_join)
  

# Trim unneeded fields ----------------------------------------------------

property <- 
  property |> 
  select(property_ID:scraped, housing:longitude, bedrooms, city, 
         ab_property:ab_host, ha_property:ha_host, CSDUID, DAUID, reg_type)

daily <- 
  daily |> 
  select(property_ID:price, listing_type:housing, city, multi, CSDUID, DAUID,
         reg_type)


# Create property_LTM -----------------------------------------------------

property_LTM <- 
  property |> 
  filter(created <= "2021-12-31", scraped >= "2021-01-01")

property_LTM <- 
  daily |> 
  filter(housing, status == "R", date >= "2021-01-01") |> 
  group_by(property_ID) |> 
  summarize(revenue = sum(price, na.rm = TRUE)) |> 
  right_join(property_LTM)


# Add tier to property ----------------------------------------------------

# Attach CMHC zone to properties ------------------------------------------

# Get the row number of the according CMHC zones (st_intersects twice as fast)
row <-
  property |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(32610) |>
  st_intersects(select(CMA, type), sparse = TRUE) |>
  as.numeric()

property$tier <-
  map_chr(row, ~{if (is.na(.x)) return(NA) else CMA$tier[.x]})

property <- mutate(property, tier = if_else(is.na(tier), "NU", tier))


# Save output -------------------------------------------------------------

qs::qsavem(property, daily, FREH, GH, host, property_LTM, exchange_rates,
           file = "output/data_processed.qsm", nthreads = availableCores())
