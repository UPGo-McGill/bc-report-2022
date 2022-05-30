#### DATA PROCESSING ###########################################################

library(tidyverse)
library(strr)
library(sf)
library(future)
plan(multisession)

qs::qload("data/data.qsm", nthreads = availableCores())
qs::qload("output/data/geometry.qsm", nthreads = availableCores())


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
         ab_property:ab_host, ha_property:ha_host, CSDUID, DAUID)

daily <- 
  daily |> 
  select(property_ID:price, listing_type:housing, city, multi, CSDUID, DAUID)


# Add tier to property ----------------------------------------------------

# Attach CMHC zone to properties ------------------------------------------

# Get the row number of the according CMHC zones (st_intersects twice as fast)
row <-
  property |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(32610) |>
  st_intersects(select(CSD, tier), sparse = TRUE) |>
  as.numeric()

property$tier <-
  map_chr(row, ~{if (is.na(.x)) return(NA) else CSD$tier[.x]})

property <- mutate(property, tier = if_else(is.na(tier), "NU", tier))


# Add tier to daily, and delete the IDs not in property -------------------

daily <- 
  daily |> 
  filter(property_ID %in% property$property_ID) |> 
  left_join(select(property, property_ID, tier), by = "property_ID")


# Add daily status and tier to GH -----------------------------------------

library(sf)
library(data.table)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession)

GH <- 
  GH |> 
  st_transform(32610) |> 
  st_join(select(CSD, tier)) |> 
  relocate(tier, .before = geometry)

GH <- 
  GH |> select(-data)

daily_GH <-
  daily |> 
  filter(property_ID %in% unique(unlist(GH$property_IDs)))

setDT(daily_GH)

daily_GH <- daily_GH %>% select(property_ID:status)

status_fun <- function(x, y) {
  status <- unique(daily_GH[date == x & property_ID %in% y, status])
  fcase("R" %in% status, "R", "A" %in% status, "A", "B" %in% status, "B")
}

status <- foreach(i = 1:nrow(GH), .combine = "c") %dopar% {
  status_fun(GH$date[[i]], GH$property_IDs[[i]])
  }

GH$status <- status
GH <- GH %>% select(ghost_ID, date, status, host_ID:data, tier, geometry)


# Save output -------------------------------------------------------------

qs::qsavem(property, daily, FREH, GH, host, property_LTM, exchange_rates,
           file = "output/data_processed.qsm", nthreads = availableCores())
