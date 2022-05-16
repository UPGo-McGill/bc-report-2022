#### FACTS #####################################################################

library(slider)

### 1. The facts about short-term rentals in BC ################################

## Basic facts

# 38,200 STRs active each day in BC in 2021, operated by 22,100 hosts

(daily_average_listings <- 
  daily |>  
  filter(date >= "2021-01-01") |> 
  group_by(date) |> 
  count() |> 
  pull(n) |>  
  mean())

(daily_average_hosts <- 
  daily |> 
  filter(date >= "2021-01-01") |> 
  group_by(date) |> 
  distinct(host_ID) |> 
  count() |> 
  pull(n) |>  
  mean(na.rm = FALSE))

# Earned $710 million ($12,900 per listing, although the median was $9,800)

(LTM_revenue <-
  daily |> 
  filter(date >= "2021-01-01", status == "R") |> 
  summarize(revenue = sum(as.numeric(price), na.rm = TRUE)) |> 
  pull(revenue))

(LTM_revenue_per_listing <- LTM_revenue / 
  nrow(filter(property, created <= "2021-12-31", scraped >= "2021-01-01")))

(LTM_revenue_median <- 
  daily |> 
  filter(date >= "2021-01-01", status == "R") |> 
  group_by(property_ID) |> 
  summarize(revenue = sum(as.numeric(price), na.rm = TRUE)) |> 
  pull(revenue) |> 
  median())

# Mostly entire homes (81.1%) and actual housing (93.2%), and half commercially
# operated (46.2%)

(EH_percentage <- 
  mean(property_LTM$listing_type == "Entire home/apt", na.rm = TRUE))

(housing_percentage <- mean(property_LTM$housing))

(ML_percentage <- 
  daily |> 
  filter(date >= "2021-01-01", status %in% c("A", "R")) |> 
  pull(multi) |> 
  mean())

# Mostly Airbnb (74.5%), with remainder Vrbo (13.1%) and both (12.3%)

(ab_alone <- sum(is.na(property_LTM$ha_property)) / nrow(property_LTM))
(ha_alone <- sum(is.na(property_LTM$ab_property)) / nrow(property_LTM))
(both_ab_ha <- 
  property_LTM |> 
  filter(!is.na(ab_property), !is.na(ha_property)) |> 
  nrow() |> 
  (\(x) x / nrow(property_LTM))())


# 35,400 housing STRs active each day in BC, operated by 21,300 hosts

(daily_average_listings_housing <- 
  daily |>  
  filter(housing, date >= "2021-01-01") |> 
  group_by(date) |> 
  count() |> 
  pull(n) |>  
  mean())

(daily_average_hosts_housing <- 
  daily |> 
  filter(housing, date >= "2021-01-01") |> 
  group_by(date) |> 
  distinct(host_ID) |> 
  count() |> 
  pull(n) |>  
  mean(na.rm = FALSE))


# Earned $670 million ($13,000 per listing, although the median was $10,100)

(LTM_revenue_housing <-
  daily |> 
  filter(housing, date >= "2021-01-01", status == "R") |> 
  summarize(revenue = sum(as.numeric(price), na.rm = TRUE)) |> 
  pull(revenue))

(LTM_revenue_per_listing_housing <- LTM_revenue_housing / 
  nrow(filter(property_LTM, housing)))

(LTM_revenue_median_housing <- 
  daily |> 
  filter(housing, date >= "2021-01-01", status == "R") |> 
  group_by(property_ID) |> 
  summarize(revenue = sum(as.numeric(price), na.rm = TRUE)) |> 
  pull(revenue) |> 
  median())


## How are STRs distributed throughout Florida?

# Map of listings

points <- 
  property_LTM |> 
  filter(housing) |> 
  mutate(revenue = if_else(is.na(revenue), as.numeric(1), as.numeric(revenue))) |> 
  strr_as_sf(32610)

listings_map <-
  points |> 
  ggplot() +
  geom_sf(data = CSD, colour = "white", fill = "grey90") +
  geom_sf(aes(colour = listing_type, size = revenue), alpha = 0.05) +
  scale_size(range = c(.1, 5), guide = "none") +
  scale_color_discrete(name = "Listing type", 
                       guide = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0, 0),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/listings_map.png", plot = listings_map, width = 6, height = 6)


# Map of listings per CSD dwellings

points_CSD <- 
  points |> 
  st_join(CSD) |> 
  st_drop_geometry() |> 
  count(GeoUID) |> 
  left_join(select(CSD, GeoUID, Dwellings)) |> 
  st_as_sf()

CSD_map <- 
  points_CSD |> 
  filter(n > 50) |> 
  ggplot() +
  geom_sf(data = CSD, colour = "grey90", fill = "white") +
  geom_sf(aes(fill = n / Dwellings), lwd = 0) +
  scale_fill_viridis_b(name = "Listings per dwelling", breaks = 0:5 * 0.02,
                       limits = c(0, 0.1), oob = scales::squish,
                       labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0, 0),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/CSD_map.png", plot = CSD_map, width = 6, height = 6)


## Map of top locations

top_cities <- 
  property |> 
  filter(scraped >= "2021-01-01", !is.na(city)) |> 
  count(city, sort = TRUE) |> 
  slice(1:8) |> 
  pull(city)

points_CT <- 
  points |> 
  filter(city %in% top_cities) |> 
  st_join(CT) |> 
  st_drop_geometry() |> 
  count(city, GeoUID) |> 
  left_join(select(CT, GeoUID, CSD_UID, Dwellings)) |> 
  st_as_sf() |> 
  left_join(select(st_drop_geometry(CSD), CSD_UID = GeoUID, city = name)) |> 
  mutate(city = str_remove(city, " \\(.*\\)$"))

points_DA <- 
  points |> 
  filter(city %in% top_cities) |> 
  st_join(DA) |> 
  st_drop_geometry() |> 
  count(city, GeoUID) |> 
  left_join(select(DA, GeoUID, CSD_UID, Dwellings)) |> 
  st_as_sf() |> 
  left_join(select(st_drop_geometry(CSD), CSD_UID = GeoUID, city = name)) |> 
  mutate(city = str_remove(city, " \\(.*\\)$"))

top_maps <- map(top_cities, ~{
  
  dat <- if (.x == "Whistler") points_DA else points_CT
  
  ggplot() +
    geom_sf(data = CSD, fill = "grey90", lwd = 0) +
    geom_sf(data = filter(dat, city == .x), aes(fill = n / Dwellings), lwd = 0) +
    scale_fill_viridis_b(name = "Listings per dwelling", breaks = 0:5 * 0.02,
                         limits = c(0, 0.1), oob = scales::squish,
                         labels = scales::percent) +
    upgo::gg_bbox(filter(dat, city == .x)) +
    ggtitle(.x) +
    theme_minimal() +
    theme(text = element_text(family = "Futura"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 8),
          plot.background = element_rect(fill = "white", colour = "transparent"))
})

top_maps_combined <- cowplot::plot_grid(plotlist = top_maps, nrow = 2)

ggsave("output/top_maps.png", plot = top_maps_combined, width = 12, height = 6)

# Graph of average daily listings
ab_daily_listings_graph <-
  daily |>  
  filter(housing, str_starts(property_ID, "ab-"), status %in% c("A", "R")) |> 
  group_by(date) |> 
  count() |> 
  ungroup() |> 
  mutate(n = slider::slide_dbl(n, mean, .before = 6)) |> 
  filter(date >= "2017-01-01") |> 
  ggplot(aes(date, n)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::comma) +
  ggtitle("Active Airbnb listings per day") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_daily_listings_graph.png", plot = ab_daily_listings_graph,
       width = 7, height = 6)

# Graph of average daily listings by reg_type
ab_daily_listings_reg_type_graph <-
  daily |>  
  filter(housing, str_starts(property_ID, "ab-"), status %in% c("A", "R")) |> 
  count(date, reg_type) |> 
  group_by(reg_type) |> 
  mutate(n = slider::slide_dbl(n, mean, .before = 6)) |> 
  ungroup() |> 
  filter(date >= "2017-01-01") |> 
  ggplot(aes(date, n, colour = reg_type)) +
  geom_line(size = 1.5) +
  scale_y_continuous(label = scales::comma) +
  scale_color_brewer("City type", palette = "Set2") +
  ggtitle("Active Airbnb listings per day") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_daily_listings_reg_type.png", 
       plot = ab_daily_listings_reg_type_graph, width = 7, height = 6)

# Graph of daily revenue
ab_revenue <- 
  daily |> 
  filter(housing, status == "R", str_starts(property_ID, "ab-")) |> 
  group_by(reg_type, date) |> 
  summarize(rev = sum(price, na.rm = TRUE)) |> 
  group_by(reg_type) |> 
  mutate(rev = slider::slide_dbl(rev, mean, .before = 29)) |> 
  ungroup()

ab_revenue_graph <-
  ab_revenue |> 
  group_by(date) |> 
  summarize(rev = sum(rev, na.rm = TRUE)) |> 
  filter(date >= "2017-01-01") |> 
  ggplot(aes(date, rev)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::dollar_format(scale = 1/1000000, 
                                                   suffix = " mil")) +
  ggtitle("Daily Airbnb host revenue") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_revenue_graph.png", plot = ab_revenue_graph, width = 7, height = 6)

# Graph of cumulative annual revenue by reg_type
ab_revenue_reg_type_graph <- 
  ab_revenue |> 
  filter(date >= "2017-01-01") |> 
  ggplot(aes(date, rev, colour = reg_type)) +
  geom_line(size = 1.5) +
  scale_y_continuous(label = scales::dollar_format(scale = 1/1000000, 
                                                   suffix = " mil")) +
  scale_color_brewer("City type", palette = "Set2") +
  ggtitle("Daily Airbnb host revenue") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_revenue_reg_type_graph.png", plot = ab_revenue_reg_type_graph,
       width = 7, height = 6)

# Graph of daily pricing
ab_pricing <- 
  daily |> 
  filter(housing, status == "R", listing_type == "Entire home/apt", 
         str_starts(property_ID, "ab-")) |> 
  group_by(date) |> 
  summarize(price = mean(price, na.rm = TRUE)) |> 
  mutate(price = slider::slide_dbl(price, mean, .before = 29)) |> 
  filter(date >= "2017-01-01")

ab_pricing_reg_type <- 
  daily |> 
  filter(housing, status == "R", listing_type == "Entire home/apt", 
         str_starts(property_ID, "ab-")) |> 
  group_by(reg_type, date) |> 
  summarize(price = mean(price, na.rm = TRUE)) |> 
  group_by(reg_type) |> 
  mutate(price = slider::slide_dbl(price, mean, .before = 29)) |> 
  ungroup() |> 
  filter(date >= "2017-01-01")

ab_pricing_graph <- 
  ab_pricing %>% 
  filter(date >= "2017-01-01") %>%
  ggplot(aes(date, price)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::dollar_format(10)) +
  ggtitle("Daily Airbnb EH price") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_pricing_graph.png", plot = ab_pricing_graph, width = 7, height = 6)

# Graph of daily pricing by reg_type
ab_pricing_reg_type_graph <- 
  ab_pricing_reg_type |> 
  ggplot(aes(date, price, colour = reg_type)) +
  geom_line(size = 1) +
  scale_y_continuous(label = scales::dollar_format(10)) +
  scale_color_brewer("City type", palette = "Set2") +
  ggtitle("Daily Airbnb EH price") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_pricing_reg_type_graph.png", plot = ab_pricing_reg_type_graph,
       width = 7, height = 6)

# Graph of revenue per listing
ab_listings <- 
  daily |> 
  filter(housing, str_starts(property_ID, "ab-")) |> 
  count(reg_type, date) |> 
  ungroup() |> 
  group_by(reg_type) |> 
  mutate(n = slider::slide_dbl(n, mean, .before = 29)) |> 
  ungroup() |> 
  filter(date >= "2017-01-01")

ab_revenue <- 
  ab_revenue |> 
  inner_join(ab_listings) |> 
  mutate(rev_per_listing = rev / n)

ab_rev_per_listing_graph <- 
  ab_revenue |> 
  group_by(date) |> 
  summarize(rev_per_listing = sum(rev) / sum(n)) |> 
  mutate(rev_per_listing = slider::slide_dbl(
    rev_per_listing, mean, .before = 29)) |> 
  ggplot(aes(date, rev_per_listing)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::dollar_format()) +
  ggtitle("Daily Airbnb revenue per listing") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_rev_per_listing_graph.png", 
       plot = ab_rev_per_listing_graph, width = 7, height = 6)

# Graph of revenue per listing by reg_type
ab_rev_per_listing_reg_type_graph <- 
  ab_revenue |> 
  group_by(reg_type) |>  
  mutate(rev_per_listing = slider::slide_dbl(
    rev_per_listing, mean, .before = 29)) |> 
  ggplot(aes(date, rev_per_listing, colour = reg_type)) +
  geom_line(size = 1) +
  scale_y_continuous(label = scales::dollar_format()) +
  scale_color_brewer("City type", palette = "Set2") +
  ggtitle("Daily Airbnb revenue per listing") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/ab_rev_per_listing_reg_type_graph.png", 
       plot = ab_rev_per_listing_reg_type_graph, width = 7, height = 6)

# Commercial operators
ML_percentages <- 
  daily |> 
  filter(housing, status != "B", str_starts(property_ID, "ab-")) |> 
  group_by(date, reg_type) |> 
  summarize(n = n(), ML_percentage = mean(multi),
            tot_revenue = sum(as.numeric(price[status == "R"])),
            ML_revenue = sum(as.numeric(price[status == "R" & multi]))) |> 
  ungroup()

# Commercial operator listings graph
commercial_operator_graph <-
  ML_percentages %>% 
  group_by(date) %>% 
  summarize(ML_percentage = sum(ML_percentage * n) / sum(n)) %>% 
  mutate(ML_percentage = slider::slide_dbl(ML_percentage, mean, 
                                           .before = 13)) %>% 
  filter(date >= "2017-01-01") %>% 
  ggplot(aes(date, ML_percentage)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::percent) +
  ggtitle("Percentage of Airbnb listings which are multi-listings") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/commercial_operator_graph.png", 
       plot = commercial_operator_graph, width = 7, height = 6)

# Commercial operator listings graph by reg_type
commercial_operator_reg_type_graph <-
  ML_percentages |> 
  group_by(reg_type) |>  
  mutate(ML_percentage = slider::slide_dbl(ML_percentage, mean, 
                                           .before = 13)) %>% 
  ungroup() %>% 
  filter(date >= "2017-01-01") %>% 
  ggplot(aes(date, ML_percentage)) +
  geom_line(aes(colour = reg_type), size = 1) +
  scale_color_brewer("City type", palette = "Set2") +
  scale_y_continuous(label = scales::percent) +
  ggtitle("Percentage of Airbnb listings which are multi-listings") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/commercial_operator_reg_type_graph.png", 
       plot = commercial_operator_reg_type_graph, width = 7, height = 6)

# Listing type
listing_type <- 
  daily |> 
  filter(housing, status == "R", str_starts(property_ID, "ab-")) |> 
  group_by(date) |> 
  count(listing_type) |> 
  mutate(pct = n / sum(n)) |> 
  ungroup()

# Listing type
listing_type_reg_type <- 
  daily |> 
  filter(housing, status == "R", str_starts(property_ID, "ab-")) |> 
  group_by(reg_type, date) |> 
  count(listing_type) |> 
  mutate(pct = n / sum(n)) |> 
  ungroup()

# Listing type graph
listing_type_graph <- 
  listing_type %>%  
  filter(listing_type == "Entire home/apt") %>%
  mutate(pct = slider::slide_dbl(pct, mean, .before = 13)) %>%
  filter(date >= "2017-01-01") %>%
  ggplot(aes(date, pct)) +
  geom_line(colour = "#225EA8", size = 1.5) +
  scale_y_continuous(label = scales::percent) +
  ggtitle("Daily EH Airbnb reservations as percentage of total") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/listing_type_graph.png", 
       plot = listing_type_graph, width = 7, height = 6)

# Listing type graph by reg_type
listing_type_reg_type_graph <-
  listing_type_reg_type %>%  
  filter(listing_type == "Entire home/apt") %>%
  group_by(reg_type) %>%
  mutate(pct = slider::slide_dbl(pct, mean, .before = 13)) %>%
  ungroup() %>%
  filter(date >= "2017-01-01") %>%
  ggplot(aes(date, pct, colour = reg_type)) +
  geom_line(size = 1) +
  scale_color_brewer("City type", palette = "Set2") +
  scale_y_continuous(label = scales::percent) +
  ggtitle("Daily EH Airbnb reservations as percentage of total") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/listing_type_reg_type_graph.png", 
       plot = listing_type_reg_type_graph, width = 7, height = 6)


# How host revenue breaks down
revenue_colour <- colorRampPalette(c("#FF6600", "#FFCC66", "#CC6699", "#3399CC", 
                                     "#074387"))(10)

host_deciles <- 
  daily %>% 
  filter(housing, status == "R", !is.na(host_ID), date >= "2021-01-01") %>% 
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>% 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) %>% 
  select(-all) %>% 
  pivot_longer(everything(), names_to = "percentile", values_to = "value") %>% 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) %>% 
  mutate(perfect_distribution = 0.1,
         decile = 1:10,
         dummy_1 = perfect_distribution,
         dummy_2 = value) %>%  
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) %>%
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") %>% 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) %>% 
  group_by(position) %>% 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) %>% 
  ungroup() %>% 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

# Host revenue percentile graph
host_revenue_percentile_graph <- 
  host_deciles %>% 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  geom_text(aes(x = 0.02, y = absolute_val, label = display_percentile), 
            data = filter(host_deciles, position == 0, decile <= 2),
            family = "Futura", hjust = 0) +
  geom_text(aes(x = 0.98, y = absolute_val, label = display_val), 
            data = filter(host_deciles, position == 1, decile <= 2),
            family = "Futura", hjust = 1) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/host_revenue_percentile_graph.png", 
       plot = host_revenue_percentile_graph, width = 8, height = 6)

host_deciles_reg_type <- 
  daily |>  
  filter(housing, status == "R", !is.na(host_ID), date >= "2021-01-01") %>% 
  group_by(reg_type, host_ID) %>%
  summarize(rev = sum(price)) %>% 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) %>% 
  select(-all) %>%  
  pivot_longer(-reg_type, names_to = "percentile", values_to = "value") %>% 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) %>% 
  mutate(perfect_distribution = 0.1,
         decile = rep(1:10, 5),
         dummy_1 = perfect_distribution,
         dummy_2 = value) %>%  
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) %>%
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") %>% 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) %>% 
  group_by(reg_type, position) %>% 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) %>% 
  ungroup() %>% 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

# Host revenue percentile reg_type graph
host_revenue_percentile_reg_type_graph <-
  host_deciles_reg_type %>% 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  facet_wrap(~ reg_type, nrow = 2) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/host_revenue_percentile_reg_type_graph.png", 
       plot = host_revenue_percentile_reg_type_graph, width = 12, height = 6)

# FREH graph
FREH_graph <- 
  FREH %>% 
  filter(FREH) |> 
  count(date) %>% 
  mutate(n = slider::slide_dbl(n, mean, .before = 13)) %>% 
  filter(date >= "2018-01-01") %>% 
  ggplot(aes(date, n)) +
  geom_line(size = 1.5, colour = "#335DA3") +
  scale_y_continuous(label = scales::comma) +
  ggtitle("FREH listings in BC") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/FREH_graph.png", plot = FREH_graph, width = 7, height = 6)

# FREH graph by reg_type
FREH_reg_type_graph <- 
  FREH %>% 
  filter(FREH) |> 
  left_join(select(property, property_ID, reg_type)) |> 
  count(date, reg_type) %>% 
  group_by(reg_type) %>% 
  mutate(n = slider::slide_dbl(n, mean, .before = 13)) %>% 
  ungroup() %>% 
  filter(date >= "2018-01-01") %>% 
  ggplot(aes(date, n, colour = reg_type)) +
  geom_line(size = 1.5) +
  scale_color_brewer("City type", palette = "Set2") +
  scale_y_continuous(label = scales::comma) +
  ggtitle("FREH listings in BC") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/FREH_reg_type_graph.png", plot = FREH_reg_type_graph, width = 7, 
       height = 6)

# FREH maps by MSA
FREH_CTs <- 
  property %>% 
  st_join(CTs) %>% 
  st_drop_geometry() %>% 
  select(property_ID, GEOID) %>% 
  right_join(filter(FREH, date == "2021-05-31")) %>%
  count(GEOID) %>%
  inner_join(CTs) %>% 
  filter(!is.na(housing_units), housing_units > 9) |> 
  st_as_sf()

FREH_metro_maps_2 <- map(metro_areas$NAMELSAD, ~{
  ggplot() +
    geom_sf(data = counties, fill = "grey90", lwd = 0) +
    geom_sf(data = FREH_CTs, 
            aes(fill = n/housing_units), lwd = 0) +
    scale_fill_viridis_b(option = "D", direction = -1, limits = c(0, 0.02), 
                         oob = scales::squish, label = scales::percent) +
    gg_bbox(filter(metro_areas, NAMELSAD == .x)) +
    ggtitle(filter(metro_areas, NAMELSAD == .x)$name_short) +
    theme_minimal() +
    theme(text = element_text(family = "Futura-Medium"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 8))
})

FREH_metro_maps_combined_2 <- 
  cowplot::plot_grid(plotlist = FREH_metro_maps_2, nrow = 2)

ggsave("output/2021_FREH_metro_maps_2.png",
       plot = FREH_metro_maps_combined_2, width = 12, height = 6)

FREH_metro_maps_5 <- map(metro_areas$NAMELSAD, ~{
  ggplot() +
    geom_sf(data = counties, fill = "grey90", lwd = 0) +
    geom_sf(data = FREH_CTs, 
            aes(fill = n/housing_units), lwd = 0) +
    scale_fill_viridis_b(option = "D", direction = -1, limits = c(0, 0.05), 
                         oob = scales::squish, label = scales::percent) +
    gg_bbox(filter(metro_areas, NAMELSAD == .x)) +
    ggtitle(filter(metro_areas, NAMELSAD == .x)$name_short) +
    theme_minimal() +
    theme(text = element_text(family = "Futura-Medium"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 8))
})

FREH_metro_maps_combined_5 <- 
  cowplot::plot_grid(plotlist = FREH_metro_maps_5, nrow = 2)

ggsave("output/2021_FREH_metro_maps_5.png",
       plot = FREH_metro_maps_combined_5, width = 12, height = 6)

FREH_metro_maps_10 <- map(metro_areas$NAMELSAD, ~{
  ggplot() +
    geom_sf(data = counties, fill = "grey90", lwd = 0) +
    geom_sf(data = FREH_CTs, 
            aes(fill = n/housing_units), lwd = 0) +
    scale_fill_viridis_b(option = "D", direction = -1, limits = c(0, 0.1), 
                         oob = scales::squish, label = scales::percent) +
    gg_bbox(filter(metro_areas, NAMELSAD == .x)) +
    ggtitle(filter(metro_areas, NAMELSAD == .x)$name_short) +
    theme_minimal() +
    theme(text = element_text(family = "Futura-Medium"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 8))
})

FREH_metro_maps_combined_10 <- 
  cowplot::plot_grid(plotlist = FREH_metro_maps_10, nrow = 2)

ggsave("output/2021_FREH_metro_maps_10.png",
       plot = FREH_metro_maps_combined_10, width = 12, height = 6)

FREH_legend_2 <- 
  cowplot::get_legend({
    ggplot() +
      geom_sf(data = FREH_CTs, aes(fill = n/housing_units), lwd = 0) +
      scale_fill_viridis_b(name = "FREH as % of all housing",
                           option = "D", direction = -1, limits = c(0, 0.02), 
                           oob = scales::squish, label = scales::percent) +
      theme_minimal() +
      theme(text = element_text(family = "Futura-Medium"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 8))
  })

ggsave("output/2021_FREH_legend_2.png",
       plot = FREH_legend_2, width = 3, height = 3)


FREH_legend_5 <- 
  cowplot::get_legend({
    ggplot() +
      geom_sf(data = FREH_CTs, aes(fill = n/housing_units), lwd = 0) +
      scale_fill_viridis_b(name = "FREH as % of all housing",
                           option = "D", direction = -1, limits = c(0, 0.05), 
                           oob = scales::squish, label = scales::percent) +
      theme_minimal() +
      theme(text = element_text(family = "Futura-Medium"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 8))
  })

ggsave("output/2021_FREH_legend_5.png",
       plot = FREH_legend_5, width = 3, height = 3)


FREH_legend_10 <- 
  cowplot::get_legend({
    ggplot() +
      geom_sf(data = FREH_CTs, aes(fill = n/housing_units), lwd = 0) +
      scale_fill_viridis_b(name = "FREH as % of all housing",
                           option = "D", direction = -1, limits = c(0, 0.1), 
                           oob = scales::squish, label = scales::percent) +
      theme_minimal() +
      theme(text = element_text(family = "Futura-Medium"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 8))
  })

ggsave("output/2021_FREH_legend_10.png",
       plot = FREH_legend_10, width = 3, height = 3)

