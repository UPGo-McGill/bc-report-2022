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

top_tier <- c("Vancouver", "Victoria", "Whistler", "Richmond", "Burnaby",
              "Surrey", "North Vancouver", "Saanich", "Coquitlam",
              "West Vancouver")

mid_tier <- c("Kelowna", "Penticton", "Vernon", "Nanaimo", "Cranbrook")

resort <- c("Parksville", "Qualicum Beach", "Courtenay", "Tofino", "Ucluelet", 
            "Osoyoos", "Oliver", "Revelstoke", "Golden", "Valemount", 
            "Clearwater", "Smithers", "Invermere", "Radium Hot Springs", 
            "Nelson", "Fernie", "Squamish")

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
                            paste0("Downtown - ", region), neighbourhood))
  })


# Import CMHC zones -------------------------------------------------------

province <- cancensus::get_census("CA16", regions = list(PR = "59"), 
                                  level = "PR", geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf()

cmhc_zones <- 
  read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp") |> 
  select(OBJECTID, NAME_EN) |> 
  st_transform(32610) |>
  filter(st_is_valid(geometry)) |> 
  st_filter(province)


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
  group_by(OBJECTID, NAME_EN) |> 
  summarize(dwellings = sum(dwellings),
            tourism_employ = sum(tourism, na.rm = TRUE) / 
              sum(all_industry, na.rm = TRUE),
            .groups = "drop")


# Attach CMHC zone to properties ------------------------------------------

# Downtown is repeated 3 times
princegeorge_downtown <- 
  cmhc_zones |> 
  filter(NAME_EN == "Downtown") |> 
  rowwise() |> 
  mutate(bbox = st_bbox(geometry)$ymax) |> 
  ungroup() |> 
  filter(bbox == max(bbox)) |> 
  pull(OBJECTID)

victoria_downtown <- 
  cmhc_zones |> 
  filter(NAME_EN == "Downtown") |> 
  rowwise() |> 
  mutate(bbox = st_bbox(geometry)$xmin) |> 
  ungroup() |> 
  filter(bbox == min(bbox)) |> 
  pull(OBJECTID)

vancouver_downtown <- 
  cmhc_zones |> 
  filter(NAME_EN == "Downtown") |> 
  filter(!OBJECTID %in% c(princegeorge_downtown, victoria_downtown)) |> 
  pull(OBJECTID)

cmhc_zones <- 
  cmhc_zones |> 
  mutate(NAME_EN = case_when(OBJECTID == princegeorge_downtown ~ 
                               "Downtown - princegeorge",
                             OBJECTID == victoria_downtown ~ 
                               "Downtown - victoria",
                             OBJECTID == vancouver_downtown ~ 
                               "Downtown - vancouver",
                             TRUE ~ NAME_EN))

# Get the row number of the according CMHC zones (st_intersects twice as fast)
row <- 
  property |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(32610) |> 
  st_intersects(select(cmhc_zones, cmhc = NAME_EN), sparse = TRUE) |> 
  as.numeric()

# A bunch have NA cmhc_zone, as there are no CMHC zones on all the territory
property$cmhc_zone <- 
  map_chr(row, ~{if (is.na(.x)) return(NA) else cmhc_zones$NAME_EN[.x]})

# reg_type are already same as tier, but let's join cmhc zones name with it and 
# take the latter data.
property <- 
  property |> 
  filter(reg_type != "Other") |>
  left_join(distinct(select(cmhc$rent, neighbourhood, tier)), 
            by = c("cmhc_zone" = "neighbourhood")) |>
  mutate(reg_type = if_else(is.na(tier), reg_type, tier)) |> 
  select(-tier)


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
    
    properties_zones <- 
      property |> 
      filter(created < paste0(y + 1, "-01-01"), 
             scraped >= paste0(y, "-01-01")) |> 
      count(cmhc_zone) |> 
      select(cmhc_zone, properties = n)
    
    str_activities <- 
      left_join(select(cmhc_zones, cmhc_zone = NAME_EN,
                       dwellings), FREH_zones, by = "cmhc_zone") |> 
      left_join(properties_zones, by = "cmhc_zone") |> 
      st_drop_geometry() |> 
      transmute(cmhc_zone,
                freh_p_dwellings = FREH / dwellings * 100,
                properties_p_dwellings = properties / dwellings * 100)
    
    units_variation <- 
      cmhc$units |> 
      filter(year %in% c(y, y - 1)) |> 
      group_by(neighbourhood) |> 
      mutate(units_variation = (total[2] - total[1]) / total[1] * 100) |>
      ungroup() |> 
      select(neighbourhood, units_variation)
    
    cmhc$vacancy |> 
      filter(year == y) |> 
      select(neighbourhood, total, tier, year) |>
      left_join(str_activities, by = c("neighbourhood" = "cmhc_zone")) |> 
      left_join(units_variation, by = "neighbourhood")
  })

cmhc_str <-
  cmhc_str |>
  mutate(year = as.factor(year))

model <- stats::lm(total ~ properties_p_dwellings + 
              freh_p_dwellings + 
              tier - 1 +
              year - 1,
            data = cmhc_str)

summary(model)

map(set_names(na.omit(unique(cmhc_str$tier))), function(tier) {
  model <- stats::lm(total ~ properties_p_dwellings + 
                       freh_p_dwellings +
                       year - 1,
                     data = filter(cmhc_str, tier == !!tier))
  summary(model)
})

summary(model)

# Addition of change in units



# # Variations between years
# 
# cmhc_str <-
#   map_dfr(2017:2021, function(year) {
#     
#     # Variation of FREH
#     FREHs <- 
#       FREH |> 
#       filter(date < paste0(year + 1, "-01-01"), 
#              date >= paste0(year - 1, "-01-01")) |> 
#       mutate(date = if_else(date < paste0(year, "-01-01"), 
#                             as.numeric(year), 
#                             as.numeric(year - 1))) |> 
#       filter(FREH) |> 
#       distinct()
#     
#     FREH_year1 <- 
#       property |> 
#       filter(created < paste0(year + 1, "-01-01"), 
#              scraped >= paste0(year, "-01-01")) |> 
#       mutate(FREH = 
#                if_else(property_ID %in% filter(FREHs, date == year)$property_ID, 
#                        TRUE, FALSE)) |> 
#       group_by(cmhc_zone) |> 
#       count(FREH) |> 
#       ungroup() |> 
#       filter(FREH) |> 
#       select(cmhc_zone, year1 = n)
#     
#     FREH_year0 <- 
#       property |> 
#       filter(created < paste0(year, "-01-01"), 
#              scraped >= paste0(year - 1, "-01-01")) |> 
#       mutate(FREH = 
#                if_else(property_ID %in% filter(FREHs, date == year - 1)$property_ID, 
#                        TRUE, FALSE)) |> 
#       group_by(cmhc_zone) |> 
#       count(FREH) |> 
#       ungroup() |> 
#       filter(FREH) |> 
#       select(cmhc_zone, year0 = n)
#     
#     FREH_variation <- 
#     left_join(FREH_year1, FREH_year0) |> 
#       transmute(cmhc_zone,
#                 FREH = (year1 - year0) / year0 * 100)
#     
#     # Variation of STR properties
#     properties_year1 <- 
#       property |> 
#       filter(created < paste0(year + 1, "-01-01"), 
#              scraped >= paste0(year, "-01-01")) |> 
#       count(cmhc_zone) |> 
#       select(cmhc_zone, year1 = n)
#     
#     properties_year0 <- 
#       property |> 
#       filter(created < paste0(year, "-01-01"), 
#              scraped >= paste0(year - 1, "-01-01")) |> 
#       count(cmhc_zone) |> 
#       select(cmhc_zone, year0 = n)
#     
#     properties_variation <- 
#       left_join(properties_year1, properties_year0) |> 
#       transmute(cmhc_zone,
#                 properties = (year1 - year0) / year0 * 100)
#     
#     str_activities <- 
#       left_join(select(cmhc_zones, cmhc_zone = NAME_EN,
#                        dwellings), FREH_variation, by = "cmhc_zone") |> 
#       left_join(properties_variation, by = "cmhc_zone") |> 
#       st_drop_geometry()
#     
#     cmhc$vacancy |> 
#       filter(year %in% c(!!year, !!year - 1)) |> 
#       group_by(neighbourhood) |> 
      # mutate(total_variation = (total[2] - total[1]) / total[1] * 100) |>
#       slice(2) |> 
      # select(neighbourhood, total_variation, tier, year = year) |>
#       inner_join(str_activities, by = c("neighbourhood" = "cmhc_zone")) |> 
#       ungroup()
#   })
# 
# cmhc_str <-
#   cmhc_str |>
#   filter(year != 2020) |> 
#   mutate(year = as.character(year))
# 
# model <- lm(total_variation ~ #FREH + 
#               properties + 
#               tier - 1 +
#               year - 1,
#             data = filter(cmhc_str, !is.infinite(total_variation)))
# 
# summary(model)


