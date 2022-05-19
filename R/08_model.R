#### MODELIZATION ##############################################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(furrr)
library(future)
library(qs)
library(sf)

qload("output/data/data_processed.qsm", nthreads = availableCores())
cmhc <- qread("output/data/cmhc.qs", nthreads = availableCores())
qload("data/geometry.qsm", nthreads = availableCores())


# Known categories --------------------------------------------------------

top_tier <- c("Vancouver", "Victoria", "Richmond", "Burnaby",
              "Surrey", "North Vancouver", "Saanich", "Coquitlam",
              "West Vancouver")

mid_tier <- c("Kelowna", "Penticton", "Vernon", "Nanaimo", "Cranbrook")

resort <- c("Parksville", "Qualicum Beach", "Courtenay", "Tofino", "Ucluelet", 
            "Osoyoos", "Oliver", "Revelstoke", "Golden", "Valemount", 
            "Clearwater", "Smithers", "Invermere", "Radium Hot Springs", 
            "Nelson", "Fernie", "Squamish", "Whistler")

resource <- c("Prince George", "Fort St. John", "Prince Rupert", "Kamloops")


# Add categories to the cmhc data -----------------------------------------

cmhc <- 
  map(cmhc, function(dat) {
    dat |> 
      mutate(tier = 
               case_when(str_detect(neighbourhood, 
                                    paste(top_tier, collapse = "|")) ~ "Top-tier",
                         str_detect(neighbourhood,
                                    paste(mid_tier, collapse = "|")) ~ "Mid-tier",
                         str_detect(neighbourhood,
                                    paste(resort, collapse = "|")) ~ "Resort",
                         str_detect(neighbourhood,
                                    paste(resource, collapse = "|")) ~ "Resource",
                         region == "nanaimo" ~ "Mid-tier",
                         region == "kamloops" ~ "Resource",
                         region == "kelowna" ~ "Mid-tier",
                         region == "princegeorge" ~ "Resource",
                         region == "vancouver" ~ "Top-tier",
                         region == "victoria" ~ "Top-tier",
                         # Abbott = similar population to Kelowna
                         region == "abbott" ~ "Mid-tier")) |> 
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
  transmute(CMAUID,
            name = CMANAME,
            type = case_when(CMATYPE == "B" ~ "CMA",
                             CMATYPE == "D" ~ "CA",
                             CMATYPE == "K" ~ "CA"))

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

# Sometimes the zones are just a bit different and overlap (like Core Area of
# Kelowna overlapping on Penticton and Vernon), causing the cmhc zones to 
# be duplicated.
cmhc_zones_cma_ca <-
  cmhc_zones |> 
  mutate(area = units::drop_units(st_area(geometry))) |> 
  st_intersection(select(CMA_shapefile, type, CMA_name = name)) |> 
  mutate(area_percentage = units::drop_units(st_area(geometry)) / area) |> 
  filter(area_percentage > 0.95) |> 
  select(-area, -area_percentage)


cmhc_zones <- 
  cmhc_zones |> 
  filter(!cmhc_zone %in% cmhc_zones_cma_ca$cmhc_zone) |> 
  mutate(type = NA, 
         CMA_name = NA,
         .before = geometry) |> 
  rbind(cmhc_zones_cma_ca)

cmhc_zones <- 
  cmhc_zones |> 
  left_join(distinct(cmhc$rent, neighbourhood, region), 
            by = c("cmhc_zone" = "neighbourhood"))

# Add dwelling numbers to zones -------------------------------------------

DA_area <- 
  DA |> 
  select(ID = GeoUID, dwellings = Dwellings,
         arts = `v_CA16_5750: 71 Arts, entertainment and recreation`,
         accomodation = `v_CA16_5753: 72 Accommodation and food services`,
         all_industry = `v_CA16_5699: All industry categories`) |> 
  transmute(ID, dwellings, tourism = arts + accomodation, all_industry) |> 
  mutate(DA_area = units::drop_units(st_area(geometry)))

cmhc_zones <- 
  cmhc_zones |> 
  mutate(zone_area = units::drop_units(st_area(geometry)))

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
         tourism = tourism * new_da_area,
         all_industry = all_industry * new_da_area) |> 
  group_by(cmhc_zone, type, CMA_name) |> 
  summarize(dwellings = sum(dwellings),
            tourism_employ = sum(tourism, na.rm = TRUE) / 
              sum(all_industry, na.rm = TRUE),
            .groups = "drop")


# Create a 'resort' type --------------------------------------------------

cmhc_zones <- 
  cmhc_zones |> 
  mutate(type = if_else(tourism_employ >= 0.125, "RES", type))


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

property$tier <- 
  map_chr(row, ~{if (is.na(.x)) return(NA) else cmhc_zones$type[.x]})

# reg_type are already same as tier, but let's join cmhc zones name with it and 
# take the latter data.
# property <- 
#   property |> 
#   filter(reg_type != "Other") |>
# left_join(distinct(select(cmhc$rent, neighbourhood, tier)), 
#           by = c("cmhc_zone" = "neighbourhood")) |>
# mutate(reg_type = if_else(is.na(tier), reg_type, tier)) |> 
# select(-tier)

# Add a STR activity indicator --------------------------------------------

# Static year numbers

cmhc_str <-
  map_dfr(2016:2021, function(y) {
    
    FREH_year <- 
      FREH |> 
      filter(date < paste0(y + 1, "-01-01"), date >= paste0(y, "-01-01")) |> 
      filter(FREH) |> 
      distinct(property_ID) |> 
      pull()
    
    FREH_zones <- 
      property |> 
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
      select(neighbourhood, units_variation)
    
    tourism_employment <- 
      cmhc_zones |> 
      st_drop_geometry() |> 
      transmute(cmhc_zone, tourism_employ = tourism_employ * 100)
    
    out <- 
    cmhc$rent |> 
      filter(year == y) |> 
      select(neighbourhood, year, total) |>
      left_join(str_activities, by = c("neighbourhood" = "cmhc_zone")) |> 
      left_join(units_variation, by = "neighbourhood") |> 
      left_join(tourism_employment, by = c("neighbourhood" = "cmhc_zone")) |> 
      left_join(select(cmhc_zones, cmhc_zone, type), 
                by = c("neighbourhood" = "cmhc_zone"))
    
    
  })

model <- stats::lm(total ~ avg_activity_p_dwellings + 
                     # units_variation +
                     # tourism_employ +
                     # freh_p_dwellings +
                     year +
                     type - 1,
                   data = filter(cmhc_str, !is.na(type)))

summary(model)

# map(set_names(na.omit(unique(cmhc_str$type))), function(t) {
#   model <- stats::lm(total ~ avg_activity_p_dwellings + 
#                        # freh_p_dwellings +
#                        year,
#                      data = filter(cmhc_str, type == t))
#   summary(model)
# })

