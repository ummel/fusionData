library(tidyverse)
library(RSocrata)
source("R/utils.R")

# All census tracts
tracts <- fst::read_fst("geo-processed/concordance/geo_concordance.fst") %>%
  select(state, county10, tract10) %>%
  distinct()

# BTS OData URL for the LATCH 2017 dataset
# This was obtained by going to: https://data.bts.gov/Research-and-Statistics/Local-Area-Transportation-Characteristics-by-House/va72-z8hz
#   and clicking the three dots drop down in top right and selecting "Access Data via OData"
# Using this approach rather than download and store the raw data, because the raw data file is quite large
url <- "https://data.bts.gov/api/odata/v4/va72-z8hz"
d <- read.socrata(url)

# An XLS data dictionary is available in Appendix D:
# https://www.bts.gov/latch-2017-methodology-appendix-d

# Extract and process desired variables
# LATCH 2017 is based on the 2017 version of the ORNL NHTS, so the 'vintage' variable is set to 2017 below
# Retaining core model outputs reflecting weekday, average per-household values for:
# est_pmiles: person-miles
# est_ptrp: person-trips
# est_vmiles: vehicle-miles
# est_vtrp: vehicle-trips
latch <- d %>%
  mutate(
    vintage = 2017L,
    geocode = str_pad(geocode, width = 11, pad = 0),
    state = substring(geocode, 1, 2),
    county10 = substring(geocode, 3, 5),
    tract10 = substring(geocode, 6, 11),
    urban_group = case_match(urban_group,  # Tract level of "urbanicity"
                             1 ~ "urban",
                             2 ~ "suburban",
                             3 ~ "rural"),
    urban_group = factor(urban_group, levels = c("urban", "suburban", "rural"), ordered = TRUE),
  ) %>%
  select(state, county10, tract10, vintage, urban_group, est_pmiles:est_vtrp) %>%
  inner_join(tracts, by = join_by(state, county10, tract10)) %>%   # Just to remove tracts not in the concordance data
  mutate_if(is.numeric, cleanNumeric, tol = 0.001)

saveRDS(latch, "geo-processed/BTS-LATCH/BTS-LATCH_2017_processed.rds")
