library(tidyverse)
library(data.table)
library(labelled)
library(sf)
source("R/utils.R")

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

geocorr.file <- "geo-raw/concordance/geocorr2022/geocorr2022_2606200354.csv.zip"  # 2022

# Read only first row to get column information
meta <- data.table::fread(file = geocorr.file, nrow = 1)

# Read and format full file
d <- data.table::fread(file = geocorr.file,
                       skip = 2,
                       col.names = names(meta),
                       colClasses = list(character = 1:(ncol(meta) - 2))) %>%
  labelled::set_variable_labels(.labels = unlist(meta[1, ]))

# Replace literal empty strings ("") with NA for character type columns
# fread() does not convert empty strings to NA, as they are ambiguous
for (i in 1:ncol(d)) {
  x <- d[[i]]
  if (is.character(x)) set(d, j = i, value = na_if(x, ""))
}

#----------

# 2020 vintage (2022 geocorr)
# Rename columns to include a year identifier (except for state)
d <- d %>%
  rename(puma20 = puma22,
         county20 = county,
         cousub20 = cousub23,
         tract20 = tract,
         bg20 = blockgroup,
         zcta20 = zcta,
         ur20 = ur,
         cbsa20 = cbsa20,
         cbsatype20 = cbsatype20,
         metdiv20 = metdiv20,
         csa20 = csa20,
         hus = hus20) %>%
  select(state:zcta20, cbsa20:csa20, ur20, hus, afact) %>%
  filter(as.integer(state) <= 56)  # Restrict to 50 states and D.C.

#----------

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
  mutate_at(vars(starts_with('cbsatype')), ~ ifelse(.x == " ", "None", substring(.x, 1, 3))) %>%
  mutate_at(vars(starts_with('cbsatype')), ~ ifelse(.x != "None", paste0(.x, "ro"), .x)) %>%
  mutate_all(~ ifelse(grepl("^(9+|\\s*)$", .x), NA, .x)) |>  # Replace all "999", etc. (all nines) and whitespace (empty) with NA
  mutate(hus = replace_na(hus, 0L))

#----------

# New for 2020 vintage
# Convert Connecticut 'county20' FIPS from new planning region codes to the old county FIPS codes
# This was not necessary previously, because Geocorr only started using the new planning region codes in the 2022 vintage of Geocorr
# It might be better to use the new codes, but it might create compatibility issues with existing codebase
d[state == "09", county20 := ct_planning_to_county_fips(county20)]

#----------

# Replace any NA values with string "None"
# This is to allow clear distinction in survey microdata between NA (unknown) and NA (not applicable); the latter should be set to "None" when survey is processed
# For example, geographies not affiliated with a CBSA are set to "None" rather than NA
d[is.na(d)] <- "None"

# Assign clean variable labels
stopifnot(all(names(d) == names(vlabs)))
labelled::var_label(d) <- vlabs

# Create final 'geocorr' data frame
geocorr <- d %>%
  filter(hus > 0) %>%
  rename(puma_weight = hus)

#----------

# Assign NCDC climate division, by block group
# Climate divisions are only defined for the Lower 48 states
# Custom codes 4900 and 5000 are introduced for Alaska and Hawaii, respectively

# Block group centroids
data(bg_centroids_2020, package = "fusionData")

# Shapefile of climate division boundaries
climdiv <- st_read("geo-raw/climate/CONUS_CLIMATE_DIVISIONS.shp/GIS.OFFICIAL_CLIM_DIVISIONS.shp") %>%
  st_make_valid() %>%
  mutate(climate_division = str_pad(CLIMDIV, width = 4, pad = 0)) %>%
  select(climate_division) %>%
  st_transform(crs = st_crs(bg_centroids_2020))

# Create initial climate division assignment
ind <- st_nearest_feature(bg_centroids_2020, climdiv)
cd <- climdiv$climate_division[ind]

# Assign custom climate division codes for Alaska and Hawaii
cd[bg_centroids_2020$state == "02"] <- "4900"
cd[bg_centroids_2020$state == "15"] <- "5000"

# Create crosswalk between block group and climate division
climdiv <- bg_centroids_2020 %>%
  mutate(pop20 = NULL,
         climate_division = cd) %>%
  st_drop_geometry()

# Assign variable description
var_label(climdiv$climate_division) <- "NCDC climate division with custom codes for AK and HI"

stopifnot(!anyNA(climdiv))

#----------

# RECS 2015 climate zone variables
# This links raw IECC codes to those used in RECS 2009 and 2015

recs15.iecc <- tibble(
  iecc_zone_15 = c("1A*", "2A*", "2B", "2B*", "3A", "3A*", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"),
  recs15_iecc_zone = c("1A-2A", "1A-2A", "2B", "2B", "3A", "3A", "3B-4B", "3C", "4A", "3B-4B", "4C", "5A", "5B-5C", "5B-5C", "6A-6B", "6A-6B", "7A-7B-7AK-8AK", "7A-7B-7AK-8AK"),
)

recs15.climate <- readRDS("geo-processed/climate/climate_zones_processed.rds") %>%
  rename(county20 = county10) %>% # Assumes county codes are unchanged from 2010 to 2020
  mutate(recs15_ba_zone = ifelse(ba_zone_15 %in% c('Cold', 'Very Cold'), 'Cold/Very Cold', ba_zone_15),
         recs15_ba_zone = ifelse(recs15_ba_zone %in% c('Hot-Dry', 'Mixed-Dry'), 'Hot-Dry/Mixed-Dry', recs15_ba_zone)) %>%
  left_join(recs15.iecc, by = "iecc_zone_15") %>%
  select(state, county20, starts_with("recs15_")) %>%
  labelled::set_variable_labels(.labels = c("State code", "County code (2020)", "RECS 2015 Building America climate zone", "RECS 2015 IECC climate zone"))

#----------

# RECS 2020 climate zone variables

# This links raw IECC codes to those used in RECS 2020
recs20.iecc <- tibble(
  iecc_zone_15 = c("1A*","2A*","2B","2B*","3A","3A*", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"),
  recs20_iecc_zone = c("1A", "2A","2B","2B","3A","3A","3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"),
)

recs20.climate <- readRDS("geo-processed/climate/climate_zones_processed.rds") %>%
  rename(county20 = county10) %>% # Assumes county codes are unchanged from 2010 to 2020
  mutate(recs20_ba_zone = ifelse(ba_zone_15 == 'Very Cold', 'Very-Cold', ba_zone_15)) %>%
  left_join(recs20.iecc, by = "iecc_zone_15") %>%
  select(state, county20, starts_with("recs20_")) %>%
  labelled::set_variable_labels(.labels = c("State code", "County code (2020)", "RECS 2020 Building America climate zone", "RECS 2020 IECC climate zone"))

#----------

# RECS 2024 climate zone variables

# This links raw IECC codes to those used in RECS 2024
recs24.iecc <- tibble(
  iecc_zone_21 = c("1A*","2A*","2B","2B*","3A","3A*", "3B", "3C", "4A", "4B", "4C", "5A", "5B", "5C", "6A", "6B", "7", "8"),
  recs24_iecc_zone = c("1A", "2A","2B","2B","3A","3A","3B", "3C", "4A", "4B", "4C", "5A", "5B","5C", "6A", "6B", "7", "8"),
)

recs24.climate <- readRDS("geo-processed/climate/climate_zones_processed.rds") %>%
  rename(county20 = county10) %>% # Assumes county codes are unchanged from 2010 to 2020
  mutate(recs24_ba_zone = ifelse(ba_zone_21 == 'Very Cold', 'Very-Cold', ba_zone_21)) %>%
  left_join(recs24.iecc, by = "iecc_zone_21") %>%
  select(state, county20, starts_with("recs24_")) %>%
  labelled::set_variable_labels(.labels = c("State code", "County code (2020)", "RECS 2024 Building America climate zone",  "RECS 2024 IECC climate zone"))

#----------

# Merge various datasets
result <- geocorr %>%
  left_join(state.merge, by = "state") %>%
  left_join(recs15.climate, by = c("state", "county20")) %>%
  left_join(recs20.climate, by = c("state", "county20")) %>%
  left_join(recs24.climate, by = c("state", "county20")) %>%
  left_join(climdiv, by = c("state", "county20", "tract20", "bg20")) %>%
  select(puma20, puma_weight, state, state_name, state_postal, everything(), -afact)

#----------

# Save the variable descriptions as separate file
defs <- enframe(var_label(result, unlist = TRUE, null_action = "na"), name = "variable", value = "description")
saveRDS(defs, file = "geo-processed/concordance/geo_concordance_definitions_2020.rds")

# Save output as .fst file
fst::write_fst(result, "geo-processed/concordance/geo_concordance_2020.fst", compress = 100)
