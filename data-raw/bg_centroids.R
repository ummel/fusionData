# ==============================================================================
# DATA CREATION SCRIPT: Census Block Group Population-Weighted Centroids
# ==============================================================================
# Purpose:
#   Downloads and processes US Census Bureau population-weighted mean centroids
#   for Block Groups from both the 2010 and 2020 Decennial Censuses.
#
# Role in package:
#   Generates exported `sf` point datasets (`bg_centroids_2010` and `bg_centroids_2020`)
#   stored in `/data/*.rda`. These spatial objects provide lightweight spatial
#   reference points for distance calculations, spatial joins, and geographic
#   aggregations across Census Block Groups without requiring full polygon geometries.
#
# Data Source:
#   US Census Bureau Centers of Population:
#   - 2010: https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt
#   - 2020: https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt
#
# Spatial Parameters:
#   - CRS: NAD83 (EPSG: 4269), matching standard US Census geographic boundaries
#     (e.g., `tigris` outputs such as `urban_areas()`).
#   - Extent: Filtered to FIPS state codes <= 56 (includes all 50 US States, DC,
#     and Puerto Rico; excludes island territories like Guam or American Samoa).
# ==============================================================================

library(sf)
library(tidyverse)

# ------------------------------------------------------------------------------
# 1. 2010 Block Group Centroids
# ------------------------------------------------------------------------------

# Fetch 2010 population-weighted mean centroids directly from Census Bureau
bg_centroids_2010 <- "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt" %>%
  read.csv(colClasses = c('character', 'character', 'character', 'character', NA, NA, NA)) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  rename(state = STATEFP,
         county10 = COUNTYFP,
         tract10 = TRACTCE,
         bg10 = BLKGRPCE,
         pop10 = POPULATION) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269)   # CRS matches that returned for Census geometry -- e.g. urban_areas()

# Save 2010 block group centroids 'sf' object to package /data directory
usethis::use_data(bg_centroids_2010, overwrite = TRUE)


# ------------------------------------------------------------------------------
# 2. 2020 Block Group Centroids
# ------------------------------------------------------------------------------

# Fetch 2020 population-weighted mean centroids directly from Census Bureau
bg_centroids_2020 <- "https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG.txt" %>%
  read.csv(colClasses = c('character', 'character', 'character', 'character', NA, NA, NA)) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  rename(state = STATEFP,
         county20 = COUNTYFP,
         tract20 = TRACTCE,
         bg20 = BLKGRPCE,
         pop20 = POPULATION) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269)   # CRS matches that returned for Census geometry -- e.g. urban_areas()

# Save 2020 block group centroids 'sf' object to package /data directory
usethis::use_data(bg_centroids_2020, overwrite = TRUE)


# ------------------------------------------------------------------------------
# Clean Up Workspace Environment
# ------------------------------------------------------------------------------
rm(bg_centroids_2010, bg_centroids_2020)
