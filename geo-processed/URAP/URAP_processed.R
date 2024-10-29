library(tidyverse)
source("R/utils.R")

d <- read_csv("geo-raw/URAP/working_bg_w_roads.csv") %>%
  mutate(state = substring(geoid, 1, 2),
         county10 = substring(geoid, 3, 5),
         tract10 = substring(geoid, 6, 11),
         bg10 = substring(geoid, 12, 12)) %>%
  filter(total_land > 0) %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  mutate(vintage = 2016L,  # Primary data is circa 2016
         land_value_per_km2 = as.integer(round(land_value_per_km2 / 1e3)),  # Convert to thousands and make integer
         # Road area totals, by type (km2)
         road_area_total = total_road_area_net,
         road_area_freeway = total_road_area_net1 + total_road_area_net2 + total_road_area_net3,
         road_area_major = total_road_area_net4 + total_road_area_net5 + total_road_area_net6,
         road_area_local = total_road_area_net7) %>%
  mutate_at(vars(road_area_total:road_area_local), ~ pmin(1, .x / total_land)) %>%  # Road area as share of total land area, by type
  select(state, county10, tract10, bg10, vintage, dist2center, land_value_per_km2, road_area_total:road_area_local) %>%
  mutate_if(is.numeric, cleanNumeric, tol = 0.001)

saveRDS(d, "geo-processed/URAP/URAP_processed.rds")
