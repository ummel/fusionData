library(sf)
library(tidyverse)

# Block group population-weighted centroids (2010)
bg_centroids <- "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt" %>%
  read.csv(colClasses = c('character', 'character', 'character', 'character', NA, NA, NA)) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  rename(state= STATEFP,
         county10 = COUNTYFP,
         tract10 = TRACTCE,
         bg10 = BLKGRPCE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%   # CRS matches that returned for Census geometry -- e.g. urban_areas()
  select(-POPULATION)

# Save 'sf' object to disk
saveRDS(bg_centroids, "geo-processed/concordance/bg_centroids.rds")
