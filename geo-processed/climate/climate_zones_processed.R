library(tidyverse)

#------------------

# Building America and IECC climate zones
# https://codes.iccsafe.org/public/document/IECC2015/iecc-residential-provisions
# https://www.energy.gov/sites/prod/files/2015/10/f27/ba_climate_region_guide_7.3.pdf

#------------------

# Introduced June 22, 2026
# Source: https://basc.pnnl.gov/guide-determining-climate-zone-county-data-files

pnnl <- sf::st_read("geo-raw/climate/ClimateZoneDataFiles") %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(IECC15), !is.na(IECC21)) %>%
  mutate(
    state = substring(GEOID, 2, 3),
    county10 = substring(GEOID, 4, 6),
    county10 = ifelse(state == "09", ct_planning_to_county_fips(county10), county10), # Converts to old county FIPS for Connecticut
    iecc_zone_15 = paste0(IECC15, ifelse(is.na(Moisture15), '', Moisture15),  ifelse(BA15 == 'Hot-Humid', '*', '')),
    iecc_zone_21 = paste0(IECC21, ifelse(is.na(Moisture21), '', Moisture21),  ifelse(BA21 == 'Hot-Humid', '*', '')),
    ba_zone_15 = BA15,
    ba_zone_21 = BA21
  ) %>%
  select(state, county10, iecc_zone_15, iecc_zone_21, ba_zone_15, ba_zone_21)

#------------------

# One-time download of old climate zones .csv file from philngo github repo
# https://gist.github.com/philngo/d3e251040569dba67942

old <- "geo-raw/climate/climate_zones.csv" %>%
  read.csv(colClasses = "character", na.strings = "N/A") %>%
  rename(state = State.FIPS,
         county10 = County.FIPS,
         ba_zone = BA.Climate.Zone) %>%
  mutate(iecc_zone = paste0(IECC.Climate.Zone, ifelse(is.na(IECC.Moisture.Regime), '', IECC.Moisture.Regime),  ifelse(ba_zone == 'Hot-Humid', '*', ''))) %>%
  select(state, county10, iecc_zone, ba_zone)

#------------------

# Combine the two data sources to pickup as much data/counties as possible to avoid missing entries

d <- full_join(pnnl, old, by = join_by(state, county10)) %>%
  mutate(
    iecc_zone_15 = ifelse(is.na(iecc_zone_15), iecc_zone, iecc_zone_15),
    iecc_zone_21 = ifelse(is.na(iecc_zone_21), iecc_zone, iecc_zone_21),
    ba_zone_15 = ifelse(is.na(ba_zone_15), ba_zone, ba_zone_15),
    ba_zone_21 = ifelse(is.na(ba_zone_21), ba_zone, ba_zone_21),
    vintage = 'always'
  ) %>%
  filter(as.integer(state) <= 56) %>%
  select(state, county10, vintage, iecc_zone_15, iecc_zone_21, ba_zone_15, ba_zone_21)

# Set variable labels
d <- labelled::set_variable_labels(d, .labels = c("State code", "County code (2010)", "Vintage", "IECC climate zone (2015)", "IECC climate zone (2021)", "Building America climate zone (2015)", "Building America climate zone (2021)"))

# Save processed data to disk
saveRDS(d, "geo-processed/climate/climate_zones_processed.rds")
