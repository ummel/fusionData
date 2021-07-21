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

# General crosswalk for geographic variables identified by state
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

#----------

# Unzpip the compressed geocorr file in geo-raw/concordance
unzip(zipfile = "geo-raw/concordance/geocorr2018_2116808121.csv.zip", exdir = tempdir())
geocorr.file <- list.files(path = tempdir(), pattern = "^geocorr", full.names = TRUE)

# Read only first row to get column information
meta <- data.table::fread(file = geocorr.file, nrow = 1)

# Read and format full file
d <- data.table::fread(file = geocorr.file,
                       skip = 2,
                       col.names = names(meta),
                       colClasses = list(character = 1:(ncol(meta) - 2))) %>%
  labelled::set_variable_labels(.labels = unlist(meta[1, ])) %>%
  mutate_all(na_if, y = " ")

#----------

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

#----------

# Assign NCDC climate division, by block group
# Climate divisions are only defined for the Lower 48 states
# Custom codes 4900 and 5000 are introduced for Alaska and Hawaii, respectively

# Block group centroids
data(bg_centroids, package = "fusionData")

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
climdiv <- bg_centroids %>%
  mutate(climate_division = cd) %>%
  st_drop_geometry()

# Assign variable description
var_label(climdiv$climate_division) <- "NCDC climate division with custom codes for AK and HI"

stopifnot(!anyNA(climdiv))

#----------

# Calculate population of each CBSA to create concordance with custom "cex_cbsasize" variable
# CBSA's are groups of contiguous counties and used as the primary sampling units in the CEX
# The CEX 'popsize' variable (from which 'cex_cbsasize' is constructed) assigns each CBSA to one of 5 population ranges

# county10.pop <- bg_centroids %>%
#   st_drop_geometry() %>%
#   group_by(state, county10) %>%
#   summarize(pop10 = sum(pop10), .groups = "drop")
#
# cbsasize <- geocorr %>%
#   select(state, county10, cbsa13) %>%
#   distinct() %>%
#   filter(!is.na(cbsa13)) %>%
#   left_join(county10.pop, by = c("state", "county10")) %>%
#   group_by(cbsa13) %>%
#   summarize(pop10 = sum(pop10), .groups = "drop") %>%
#   mutate(cex_cbsasize = cut(pop10, breaks = c(0, 100e3, 500e3, 1e6, 5e6, Inf), right = FALSE, labels = FALSE),
#          cex_cbsasize = c("Less than 100 thousand", "100-500 thousand", "0.5-1.0 million", "1-5 million", "More than 5 million")[cex_cbsasize]) %>%
#   select(-pop10)

#----------

# Merge various datasets
result <- geocorr %>%
  left_join(state.merge, by = "state") %>%
  left_join(recs.climate, by = c("state", "county10")) %>%
  left_join(climdiv, by = c("state", "county10", "tract10", "bg10")) %>%
  # left_join(cbsasize, by = "cbsa13") %>%
  # mutate(cex_cbsasize = ifelse(ur12 == "U" & !is.na(cbsa13), cex_cbsasize, "Rural"),
  #        cex_metro = ifelse(ur12 == "U" & !is.na(cbsa13) & cbsatype13 == "Metro", "Metro", "Not metro")) %>%
  select(puma10, puma_weight, state, state_name, state_postal, everything(), -afact)

#----------

# Save processed 'geolink' .fst file to disk
fst::write_fst(result, "geo-processed/concordance/geo_concordance.fst", compress = 100)
