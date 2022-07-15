#### April data import #########################################################

library(tidyverse)
library(strr)
library(sf)
library(qs)
library(future)
plan(multisession)


# Get April data ----------------------------------------------------------

property_april <- with_edition(1, read_csv(
  "~/Code/global-file-import/data/ca_Property_Match_2022-07-11.csv",
  col_types = cols_only(
    `Property ID` = col_character(), 
    `Listing Title` = col_character(),
    `Property Type` = col_character(),
    `Listing Type` = col_character(),
    `Created Date` = col_date(format = ""),
    `Last Scraped Date` = col_date(format = ""),
    Country = col_character(), Latitude = col_double(), 
    Longitude = col_double(), State = col_character(), 
    City = col_character(), Neighborhood = col_character(),
    `Metropolitan Statistical Area` = col_character(),
    `Currency Native` = col_character(), Bedrooms = col_double(),
    Bathrooms = col_double(), `Max Guests` = col_double(),
    `Response Rate` = col_double(), 
    `Airbnb Superhost` = col_logical(), 
    `HomeAway Premier Partner` = col_logical(),
    `Cancellation Policy` = col_character(),
    `Security Deposit (USD)` = col_double(),
    `Cleaning Fee (USD)` = col_double(),
    `Extra People Fee (USD)` = col_double(),
    `Check-in Time` = col_character(), 
    `Checkout Time` = col_character(), `Minimum Stay` = col_double(),
    `Number of Reviews` = col_double(), 
    `Number of Photos` = col_double(), 
    `Instantbook Enabled` = col_logical(), 
    `Overall Rating` = col_double(), 
    `Airbnb Property ID` = col_character(), 
    `Airbnb Host ID` = col_character(),
    `Airbnb Listing Main Image URL` = col_character(),
    `HomeAway Property ID` = col_character(),
    `HomeAway Property Manager` = col_character(),
    `HomeAway Listing Main Image URL` = col_character()
  )))

property_april <- strr::strr_process_property(property_april)
property_april <- property_april[[1]]

scraped_fix <- 
  read_csv("~/Code/global-file-import/data/fix_scraped_date.csv") |> 
  set_names(c("property_ID", "scraped_old", "scraped_new")) |> 
  mutate(property_ID = paste0("ab-", property_ID)) |> 
  filter(scraped_old != scraped_new)

prop_to_update <-
  property_april |> 
  select(property_ID, scraped) |> 
  inner_join(scraped_fix, by = "property_ID") |> 
  filter(scraped_old == scraped) |> 
  select(-scraped, -scraped_old)

property_april <-
  property_april |> 
  left_join(prop_to_update, by = "property_ID") |> 
  mutate(scraped = coalesce(scraped_new, scraped)) |> 
  select(-scraped_new)

rm(prop_to_update, scraped_fix)

upgo::upgo_connect()

property_april <- 
  property_remote %>% 
  select(property_ID, first_active, last_active) %>% 
  left_join(property_april, ., by = "property_ID", copy = TRUE) %>% 
  relocate(first_active, last_active, .after = scraped)

property_april <- 
  daily_remote %>% 
  group_by(property_ID) %>% 
  filter(property_ID %in% !!filter(property_april, is.na(created))$property_ID,
         start_date == min(start_date[status != "U"], na.rm = TRUE)) %>% 
  collect() %>% 
  select(property_ID, created2 = start_date) %>% 
  left_join(property_april, ., by = "property_ID") %>% 
  mutate(created = coalesce(created, created2)) %>% 
  select(-created2)

property_april <- 
  daily_remote %>% 
  group_by(property_ID) %>% 
  filter(property_ID %in% !!filter(property_april, is.na(scraped))$property_ID,
         end_date == max(end_date[status != "U"], na.rm = TRUE)) %>% 
  collect() %>% 
  select(property_ID, scraped2 = end_date) %>% 
  left_join(property_april, ., by = "property_ID") %>% 
  mutate(scraped = coalesce(scraped, scraped2)) %>% 
  select(-scraped2) %>% 
  filter(!is.na(scraped))

upgo_disconnect()

daily_april <- with_edition(1, read_csv(
  "/Users/dwachsmuth/Code/global-file-import/data/daily_2022_04.gz")) %>% 
  select(`Property ID`, Date, Status, `Booked Date`, `Price (USD)`,
         `Reservation ID`)

output <- strr_process_daily(daily_april, property_april)

daily_BC <- output[[1]]

daily_april_small <- 
  output[[1]] |> 
  select(property_ID:status)

property_april <-
  daily_april_small %>% 
  filter(status %in% c("A", "R")) %>% 
  group_by(property_ID) %>% 
  summarize(first = min(date),
            last = max(date)) %>%
  left_join(property_april, ., by = "property_ID") %>% 
  mutate(first_active = pmin(first_active, first, na.rm = TRUE),
         last_active = pmax(last_active, last, na.rm = TRUE)) %>% 
  select(-first, -last)

daily_created <- 
  daily_april_small %>% 
  filter(status != "U") %>% 
  group_by(property_ID) %>% 
  summarize(created_new = min(date, na.rm = TRUE))

property_april <- 
  property_april %>% 
  left_join(daily_created, by = "property_ID") %>% 
  mutate(created = coalesce(created, created_new)) %>% 
  select(-created_new)

rm(daily_created)

property_BC <- 
  property_april |> 
  filter(region == "British Columbia")

daily_BC <- 
  daily_BC |> 
  filter(property_ID %in% property_BC$property_ID)

host_BC <- 
  output[[1]] |> 
  strr_host() |> 
  filter(host_ID %in% property_BC$host_ID)

qsavem(property_BC, daily_BC, host_BC, 
       file = "output/data/data_april.qsm", nthreads = availableCores())


# Redo analysis steps on April data ---------------------------------------

qs::qload("output/data/data_april.qsm", nthreads = availableCores())
qs::qload("output/data/data_processed.qsm", nthreads = availableCores())
qs::qload("output/data/geometry.qsm", nthreads = availableCores())

# Exchange rates
exchange_rates_BC <- upgo::convert_currency(start_date = min(daily_BC$date), 
                                            end_date = max(daily_BC$date))

daily_BC <-
  daily_BC  |>
  mutate(year_month = substr(date, 1, 7)) |> 
  left_join(exchange_rates_BC, by = "year_month") |> 
  mutate(price = price * exchange_rate) |> 
  select(-year_month, -exchange_rate)

exchange_rates <- bind_rows(exchange_rates, exchange_rates_BC)

# ML
daily_BC <- strr_multi(daily_BC, host_BC)

# FREH
FREH_BC <-
  daily |> 
  select(all_of(names(daily_BC))) |> 
  bind_rows(daily_BC) |> 
  filter(date >= "2021-03-01") |> 
  strr_FREH()

# GH
GH_BC <- 
  property_BC |> 
  strr_ghost()

GH_BC <- 
  GH_BC |> 
  filter(date >= "2022-04-01")

# DAs
DA_to_join <- 
  property_BC |> 
  strr_as_sf(32610) |> 
  select(property_ID) |> 
  st_intersection(DA) |> 
  st_drop_geometry() |> 
  select(property_ID, CSDUID = CSD_UID, DAUID = GeoUID)

property_BC <- 
  property_BC |>
  left_join(DA_to_join)

daily_BC <- 
  daily_BC |> 
  left_join(DA_to_join)

# Trim fields
property_BC <- 
  property_BC |> 
  select(property_ID:scraped, housing:longitude, bedrooms, city, 
         ab_property:ab_host, ha_property:ha_host, CSDUID, DAUID)


# Add tier to property
row <-
  property_BC |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(32610) |>
  st_intersects(select(CSD, tier), sparse = TRUE) |>
  as.numeric()

property_BC$tier <-
  map_chr(row, ~{if (is.na(.x)) return(NA) else CSD$tier[.x]})

property_BC <- mutate(property_BC, tier = if_else(is.na(tier), "NU", tier))

# Add tier to daily
daily_BC <- 
  daily_BC |> 
  filter(property_ID %in% property_BC$property_ID) |> 
  left_join(select(property_BC, property_ID, tier), by = "property_ID")


# Add daily status and tier to GH -----------------------------------------

library(sf)
library(data.table)
library(doFuture)
registerDoFuture()
plan(multisession)

GH_BC <- 
  GH_BC |> 
  st_transform(32610) |> 
  st_join(select(CSD, tier)) |> 
  relocate(tier, .before = geometry)

GH_BC <- 
  GH_BC |> select(-data)

daily_GH_BC <-
  daily_BC |> 
  filter(property_ID %in% unique(unlist(GH_BC$property_IDs)))

setDT(daily_GH_BC)

daily_GH_BC <- daily_GH_BC %>% select(property_ID:status)

status_fun <- function(x, y) {
  status <- unique(daily_GH_BC[date == x & property_ID %in% y, status])
  fcase("R" %in% status, "R", "A" %in% status, "A", "B" %in% status, "B")
}

status <- foreach(i = 1:nrow(GH_BC), .combine = "c") %do% {
  status_fun(GH_BC$date[[i]], GH_BC$property_IDs[[i]])
}

GH_BC$status <- status

GH_BC <- 
  GH_BC %>% 
  select(ghost_ID, date, status, host_ID:property_IDs, tier, geometry)


# Put everything together -------------------------------------------------

qload("output/data/FREH_model.qsm", nthreads = availableCores())

property <- property_BC

daily <- 
  daily |> 
  select(-FREH_3) |> 
  bind_rows(daily_BC)

FREH <- 
  FREH |> 
  bind_rows(filter(FREH_BC, date >= "2022-04-01"))

GH <- 
  GH |> 
  filter(date <= "2022-03-31") |> 
  bind_rows(GH_BC)


# Fix housing -------------------------------------------------------------

property <- 
  property |> 
  mutate(housing = if_else(property_type %in% c(
    "Home", "Tiny home", "house", "condo", "Country house/chateau",
    "Entire vacation home", "cottage", "townhome", "apartment"), TRUE, housing))

daily <- 
  daily |> 
  select(-housing) |> 
  left_join(select(property, property_ID, housing), by = "property_ID") |> 
  relocate(housing, .after = listing_type)


# Save output -------------------------------------------------------------

qs::qsavem(property, daily, FREH, GH, file = "output/data/data_processed.qsm", 
           nthreads = availableCores())
