
# Arguments for CMHC import function --------------------------------------------------------

region <- c("vancouver", "victoria", "kelowna", "nanaimo", "kamloops", "abbott", "chili",
           "princegeorge", "bc")
data <- c("units", "rent", "vacancy")
year <- c(2016, 2017, 2018, 2019, 2020, 2021)
type <- c("bachelor", "one_bedroom", "two_bedroom", "three_bedroom", "total")
cities_to_remove <- c("Abbotsford - Mission", "Chilliwack", "Kamloops", "Kelowna",
            "Nanaimo", "Prince George", "Vancouver", "Victoria")


# Arguments for CMHC import function --------------------------------------------------------

import_cmhc <- function(region, data, year) {
  
  read_csv(paste0("data/cmhc/", region, "_", data, "_", year, ".csv"), skip = 2) %>% 
    select(...1, Bachelor, `1 Bedroom`, `2 Bedroom`, `3 Bedroom +`, Total) %>% 
    set_names(c("neighbourhood", "bachelor", "one_bedroom", "two_bedroom", "three_bedroom", "total")) %>% 
    mutate(across(type, ~ifelse(.x == "**", NA, .x))) %>%
    mutate(across(type, ~str_remove_all(.x, "\\,"))) %>% 
    mutate(across(type, ~as.numeric(.x))) %>% 
    mutate(year = year) 
}

    