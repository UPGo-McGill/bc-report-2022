#### MODELIZATION ##############################################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(furrr)
library(future)
library(qs)
library(sf)

qload("output/data_processed.qsm", nthreads = availableCores())
cmhc <- qread("output/cmhc.qs", nthreads = availableCores())
qload("data/geometry.qsm", nthreads = availableCores())


# Update CMHC names to fit with zones -------------------------------------

cmhc <- 
  map(cmhc, function(dat) {
    dat |> 
      mutate(neighbourhood = if_else(neighbourhood == "Downtown", 
                            paste0("Downtown - ", region), neighbourhood)) |> 
      # Fixing some neighbourhood names to fit with the cmhc zones
      mutate(neighbourhood = case_when(neighbourhood == "Centre" ~ "Nanaimo (Centre)",
                                       neighbourhood == "South" ~ "Nanaimo (South)",
                                       neighbourhood == "North & Periphery" ~ 
                                         "Nanaimo (North & Periphery)",
                                       neighbourhood == "Summerland DM" ~ "Summerland",
                                       TRUE ~ neighbourhood)) 
  })


# Import CMHC zones -------------------------------------------------------

cmhc_zones <- 
  read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp") |> 
  select(OBJECTID, cmhc_zone = NAME_EN) |> 
  st_transform(32610) |>
  filter(st_is_valid(geometry)) |> 
  st_filter(province)

# Downtown is repeated 3 times
princegeorge_downtown <- 
  cmhc_zones |> 
  filter(cmhc_zone == "Downtown") |> 
  rowwise() |> 
  mutate(bbox = st_bbox(geometry)$ymax) |> 
  ungroup() |> 
  filter(bbox == max(bbox)) |> 
  pull(OBJECTID)

victoria_downtown <- 
  cmhc_zones |> 
  filter(cmhc_zone == "Downtown") |> 
  rowwise() |> 
  mutate(bbox = st_bbox(geometry)$xmin) |> 
  ungroup() |> 
  filter(bbox == min(bbox)) |> 
  pull(OBJECTID)

vancouver_downtown <- 
  cmhc_zones |> 
  filter(cmhc_zone == "Downtown") |> 
  filter(!OBJECTID %in% c(princegeorge_downtown, victoria_downtown)) |> 
  pull(OBJECTID)

cmhc_zones <- 
  cmhc_zones |> 
  mutate(cmhc_zone = case_when(OBJECTID == princegeorge_downtown ~ 
                               "Downtown - princegeorge",
                             OBJECTID == victoria_downtown ~ 
                               "Downtown - victoria",
                             OBJECTID == vancouver_downtown ~ 
                               "Downtown - vancouver",
                             TRUE ~ cmhc_zone))

# Fix namings to fit with the cmhc data
cmhc_zones <- 
  cmhc_zones |> 
  mutate(cmhc_zone = case_when(cmhc_zone == "Nelson CY" ~ "Nelson", 
                             cmhc_zone == "Summerland DM" ~ "Summerland", 
                             str_starts(cmhc_zone, "James Bay") ~ "James Bay",
                             cmhc_zone == "Dundrave/W Vancouver Remainder" ~ 
                               "Dundrave/West Vancouver Remainder",
                             cmhc_zone == "Hastings/Grandview/Woodlands" ~
                               "Hastings/Sunrise/Grandview/Woodlands",
                             TRUE ~ cmhc_zone)) |> 
  group_by(cmhc_zone) |> 
  summarize()

# Keep cmhc_zones part of our collected data
cmhc_zones <- 
  cmhc_zones |> 
  filter(cmhc_zone %in% unique(cmhc$rent$neighbourhood))

# Add CSD's tier to the cmhc zone. Take the tier of the CSD that's taking
# the most space in the zone
cmhc_zones <-
  cmhc_zones |> 
  mutate(area = units::drop_units(st_area(geometry))) |> 
  st_intersection(select(CSD, name, tier)) |> 
  mutate(area_percentage = units::drop_units(st_area(geometry)) / area) |> 
  group_by(cmhc_zone) |> 
  filter(area_percentage  == max(area_percentage)) |> 
  select(-area, -area_percentage, -name) |> 
  ungroup()

# Add dwelling numbers to zones -------------------------------------------

DA_area <- 
  DA |> 
  # We only need renters
  select(ID = GeoUID, dwellings = Dwellings,
         # arts = `v_CA16_5750: 71 Arts, entertainment and recreation`,
         # accomodation = `v_CA16_5753: 72 Accommodation and food services`,
         # all_industry = `v_CA16_5699: All industry categories`,
         renter = `v_CA16_4838: Renter`,
         parent_renter = `v_CA16_4836: Total - Private households by tenure - 25% sample data`,
         # dwellings_value_avg = `v_CA16_4896: Average value of dwellings ($)`,
         # movers_5yrs = `v_CA16_6725: Movers`,
         # parent_movers_5yrs = `v_CA16_6719: Total - Mobility status 5 years ago - 25% sample data`
         ) |> 
  # mutate(tourism = arts + accomodation) |> 
  mutate(DA_area = units::drop_units(st_area(geometry)))

cmhc_zones <- 
  cmhc_zones |> 
  st_intersection(DA_area) |> 
  filter(st_is(geometry, "POLYGON") | st_is(geometry, "MULTIPOLYGON")) |>
  mutate(
    new_da_area = units::drop_units(st_area(geometry)) / DA_area,
    .before = geometry
  ) |> 
  group_by(ID) |> 
  filter(sum(new_da_area) >= 0.9) |> 
  ungroup() |> 
  mutate(dwellings = dwellings * new_da_area,
         # tourism = tourism * new_da_area,
         # all_industry = all_industry * new_da_area,
         renter = renter * new_da_area,
         parent_renter = parent_renter * new_da_area,
         # movers_5yrs = movers_5yrs * new_da_area,
         # parent_movers_5yrs = parent_movers_5yrs * new_da_area
         ) |> 
  group_by(cmhc_zone, tier) |> 
  summarize(dwellings = sum(dwellings),
            # tourism_employ = sum(tourism, na.rm = TRUE) / 
              # sum(all_industry, na.rm = TRUE),
            renter_pct = sum(renter, na.rm = TRUE) / 
              sum(parent_renter, na.rm = TRUE),
            # movers_5yrs_pct = sum(movers_5yrs, na.rm = TRUE) / 
              # sum(parent_movers_5yrs, na.rm = TRUE),
            # dwellings_value_avg = weighted.mean(dwellings_value_avg, new_da_area),
            .groups = "drop")


# Attach CMHC zone to properties ------------------------------------------

# Get the row number of the according CMHC zones (st_intersects twice as fast)
row <-
  property |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(32610) |>
  st_intersects(select(cmhc_zones, cmhc = cmhc_zone), sparse = TRUE) |>
  as.numeric()

# A bunch have NA cmhc_zone, as there are no CMHC zones on all the territory
property$cmhc_zone <-
  map_chr(row, ~{if (is.na(.x)) return(NA) else cmhc_zones$cmhc_zone[.x]})

# Add a STR activity indicator --------------------------------------------

cmhc_str <-
  map_dfr(2016:2021, function(y) {
    
    FREH_year <- 
      daily |> 
      filter(housing,
             date < paste0(y + 1, "-01-01"), date >= paste0(y, "-01-01")) |> 
      filter(FREH_3 >= 0.5) |> 
      distinct(property_ID) |> 
      pull()
    
    FREH_zones <- 
      property |> 
      filter(housing) |> 
      filter(created < paste0(y + 1, "-01-01"), 
             scraped >= paste0(y, "-01-01")) |> 
      mutate(FREH = if_else(property_ID %in% FREH_year, TRUE, FALSE)) |> 
      count(cmhc_zone, FREH) |> 
      filter(FREH) |> 
      select(cmhc_zone, FREH = n)
    
    avg_activity <- 
      daily |> 
      filter(housing,
             date < paste0(y + 1, "-01-01"), 
             date >= paste0(y, "-01-01")) |> 
      filter(status != "B") |> 
      left_join(select(property, property_ID, cmhc_zone), by = "property_ID") |> 
      group_by(date, cmhc_zone) |> 
      summarize(daily_activity = n(), .groups = "drop") |> 
      group_by(cmhc_zone) |> 
      summarize(avg_activity = sum(daily_activity) / 365)
    
    str_activities <- 
      left_join(select(cmhc_zones, cmhc_zone = cmhc_zone,
                       dwellings), FREH_zones, by = "cmhc_zone") |> 
      left_join(avg_activity, by = "cmhc_zone") |> 
      st_drop_geometry() |> 
      transmute(cmhc_zone,
                FREH,
                freh_p_dwellings = FREH / dwellings * 100,
                avg_activity,
                avg_activity_p_dwellings = avg_activity / dwellings * 100)
    
    units_variation <- 
      cmhc$units |> 
      filter(year %in% c(y, y - 1)) |> 
      group_by(neighbourhood) |> 
      mutate(units_variation = (total[2] - total[1]) / total[1] * 100) |>
      ungroup() |> 
      filter(year == y) |> 
      select(neighbourhood, units_variation)
    
    renter_pcts <- 
      cmhc_zones |> 
      st_drop_geometry() |> 
      transmute(cmhc_zone, 
                # tourism_employ = tourism_employ * 100,
                renter_pct = renter_pct * 100#,
                # movers_5yrs_pct = movers_5yrs_pct * 100,
                # dwellings_value_avg
                )
    
    # out <- 
    cmhc$rent |> 
      filter(year == y) |> 
      select(neighbourhood, year, total_rent = total) |>
      left_join(str_activities, by = c("neighbourhood" = "cmhc_zone")) |> 
      left_join(units_variation, by = "neighbourhood") |> 
      left_join(renter_pcts, by = c("neighbourhood" = "cmhc_zone")) |>
      left_join(select(cmhc_zones, cmhc_zone, tier), 
                by = c("neighbourhood" = "cmhc_zone")) |> 
      # Jambes Bay is a duplicated name (for a match with the spatial data), but 
      # it's 2 neighbourhoods! So it duplicates in the left join with units variation.
      distinct()
    
  })

cmhc_str <- 
  cmhc_str |> 
  mutate(year = year - 2016)

model <- lm(total_rent ~ #avg_activity_p_dwellings + 
              # units_variation +
              # tourism_employ +
              freh_p_dwellings +
              renter_pct +
              # movers_5yrs_pct +
              # dwellings_value_avg +
              year +
              tier - 1,
            data = cmhc_str)

summary(model)


# Rent in zones -----------------------------------------------------------

cmhc_str |> 
  filter(!is.na(freh_p_dwellings)) |> 
  mutate(year = year + 2016) |> 
  group_by(tier, year) |> 
  summarize(freh_p_dwellings = mean(freh_p_dwellings, na.rm = TRUE)) |> 
  ggplot(aes(year, freh_p_dwellings, color = tier)) +
  geom_line()


daily |> 
  filter(year(date) >= 2016) |> 
  filter(FREH_3 > 0.5) |> 
  left_join(select(property, property_ID, cmhc_zone, tier), by = "property_ID") |> 
  count(date, tier) |> 
  ggplot(aes(date, n, color = tier)) +
  geom_line()
  

# Prepare the FREH graph grouped by types ---------------------------------

# property_FREH_year <- 
#   FREH |> 
#   filter(FREH) |> 
#   mutate(date = year(date)) |> 
#   distinct(property_ID, date)



# Save --------------------------------------------------------------------

qsavem(model, cmhc_str, file = "output/model_chapter.qsm")
qs::qsavem(property, daily, FREH, GH, host, property_LTM, exchange_rates,
           file = "output/data_processed.qsm", nthreads = availableCores())

