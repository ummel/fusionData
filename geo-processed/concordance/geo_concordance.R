library(tidyverse)
library(labelled)
library(sf)

#----------

# Geocorr2018 application: https://mcdc.missouri.edu/applications/geocorr2018.html
# Help page: https://mcdc.missouri.edu/applications/docs/geocorr-help.html
# Geography glossary: https://mcdc.missouri.edu/applications/docs/maggot2014.html

# Reference table for number of digits per GEOID
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html

#----------

state.merge <- readRDS("geo-raw/miscellaneous/Geographic entities to merge on state.rds")

#----------

# RECS 'recs_iecc_zone' variable
# This links raw IECC codes to those used in RECS 2009 and 2015
recs.iecc <- tibble(
  iecc_zone = c("1A*", "2A*", "2B", "2B*", "3A", "3A*", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"),
  recs_iecc_zone = c("1A-2A", "1A-2A", "2B", "2B", "3A", "3A", "3B-4B", "3C", "4A", "3B-4B", "4C", "5A", "5B-5C", "5B-5C", "6A-6B", "6A-6B", "7A-7B-7AK-8AK", "7A-7B-7AK-8AK")
) %>%
  mutate(recs_iecc_zone = paste0("IECC climate zone", ifelse(grepl("-", recs_iecc_zone), "s ", " "), recs_iecc_zone))

recs.climate <- readRDS("geo-processed/climate/climate_zones_processed.rds") %>%
  mutate(recs_ba_zone = ifelse(ba_zone %in% c('Cold', 'Very Cold'), 'Cold/Very Cold', ba_zone),
         recs_ba_zone = ifelse(recs_ba_zone %in% c('Hot-Dry', 'Mixed-Dry'), 'Hot-Dry/Mixed-Dry', recs_ba_zone)) %>%
  left_join(recs.iecc, by = "iecc_zone") %>%
  select(state, county10, starts_with("recs_")) %>%
  labelled::set_variable_labels(.labels = c("State code", "County code (2010)", "RECS IECC climate zone", "RECS Building American climate zone"))

#-----

geocorr.file <- readr::read_csv("geo-raw/concordance/geocorr2018_2116808121.csv.zip")

# Read only first row to get column information
meta <- data.table::fread(file = geocorr.file, nrow = 1)

# Read and format full file
d <- data.table::fread(file = geocorr.file,
                       skip = 2,
                       col.names = names(meta),
                       colClasses = list(character = 1:(ncol(meta) - 2))) %>%
  labelled::set_variable_labels(.labels = unlist(meta[1, ])) %>%
  mutate_all(na_if, y = " ")

# Rename columns to include a year identifier (except for state)
d <- d %>%
  rename(puma10 = puma12,
         county10 = county,
         cousubfp10 = cousubfp,
         tract10 = tract,
         bg10 = bg,
         zcta10 = zcta5,
         sldu10 = sldu,
         sldl10 = sldl,
         ur12 = ur,
         ua12 = ua,
         cbsa15 = cbsa,
         cbsatype15 = cbsatype,
         metdiv15 = metdiv,
         csa15 = csa)

# Extract and clean up variable labels/definitions
vlabs <- labelled::var_label(d, unlist = TRUE)
x <- str_squish(gsub("\\s*\\([^\\)]+\\)","", vlabs))
y <- names(vlabs)
vlabs <- ifelse(grepl("[a-z]\\d{2}$", y), paste0(x, " (20", str_sub(y, -2, -1), ")"), x)
vlabs[length(vlabs)] <- "Housing units allocation factor (sums to 1 for each PUMA)"
names(vlabs) <- y

# Fix-up values for select variables
d <- d %>%
  mutate_at(vars(starts_with('county')), ~ substring(.x, 3, 5)) %>%
  mutate_at(vars(starts_with('tract')), ~ sub(".", "", .x, fixed = TRUE)) %>%
  mutate_at(vars(starts_with('cbsatype')), ~ ifelse(substring(.x, 1, 3) == "Met", "Metro", "Micro")) %>%
  mutate_at(vars(starts_with('sdbesttype')),toupper) %>%
  mutate_all(~ ifelse(grepl("^[9]*$", .x), NA, .x)) %>%   # Replace all "999", etc. (all nines) with NA
  mutate(hus10 = replace_na(hus10, 0L))

# Assign clean variable labels
stopifnot(all(names(d) == names(vlabs)))
labelled::var_label(d) <- vlabs

# Create final 'geocorr' data frame
geocorr <- d %>%
  filter(hus10 > 0) %>%
  rename(puma_weight = hus10)

# Ensure that no PUMA's were dropped due to rounding of allocation factors
stopifnot({
  length(unique(paste0(d$state, d$puma10))) == length(unique(paste0(geocorr$state, geocorr$puma10)))
})

#-----------------
#-----------------

# Assign NCDC climate division, by block group
# Climate divisions are only defined for the Lower 48 states
# Custom codes 4900 and 5000 are introduced for Alaska and Hawaii, respectively

# Block group centroids
bg_centroids <- readRDS("geo-processed/concordance/bg_centroids.rds")

# Shapefile of climate division boundaries
climdiv <- st_read("geo-raw/climate/CONUS_CLIMATE_DIVISIONS/GIS.OFFICIAL_CLIM_DIVISIONS.shp") %>%
  st_make_valid() %>%
  mutate(climate_division = str_pad(CLIMDIV, width = 4, pad = 0)) %>%
  select(climate_division) %>%
  st_transform(crs = st_crs(bg_centroids))

# Create initial climate division assignment
ind <- st_nearest_feature(bg_centroids, climdiv)
cd <- climdiv$climate_division[ind]

# Assign custom climate division codes for Alaska and Hawaii
cd[bg_centroids$state == "02"] <- "4900"
cd[bg_centroids$state == "15"] <- "5000"

# Create crosswalk between block group and climate division
climdiv.xwalk <- bg_centroids %>%
  mutate(climate_division = cd) %>%
  st_drop_geometry()

# Assign variable description
var_label(climdiv.xwalk$climate_division) <- "NCDC climate division with custom codes for AK and HI"

stopifnot(!anyNA(climdiv.xwalk))

#-----------------
#-----------------

# Merge various datasets
result <- geocorr %>%
  left_join(state.merge, by = "state") %>%
  left_join(recs.climate, by = c("state", "county10")) %>%
  left_join(climdiv.xwalk, by = c("state", "county10", "tract10", "bg10")) %>%
  select(puma10, puma_weight, state, state_name, state_postal, everything(), -afact)

# Save 'geolink' variable descriptions to disk
#saveRDS(var_label(result), "geo-processed/puma_concordance_dictionary.rds")

# Save processed 'geolink' .fst file to disk
fst::write_fst(result, "geo-processed/concordance/geo_concordance.fst", compress = 100)
