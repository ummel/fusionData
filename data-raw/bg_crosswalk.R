# ==============================================================================
# DATA PREPARATION: 2020 to 2010 Block Group Crosswalk (bg_crosswalk)
# ==============================================================================
# Objective:
#   Construct a clean mapping dataset connecting 2020 Census Block Groups to 2010
#   Census Block Groups, featuring intersection-level population weights derived
#   from 2020 Decennial Census total population counts and NHGIS crosswalk factors.
#
# Methodology & Sources:
#   1. 2020 Total Population: Pulled via US Census Bureau API (`tidycensus`) using
#      the 2020 Public Law 94-171 Redistricting Data Summary File (`P1_001N`).
#   2. NHGIS Geographic Crosswalk: IPUMS NHGIS 2020-to-2010 Block Group Crosswalk.
#      URL: https://www.nhgis.org/geographic-crosswalks
#      `wt_pop`: Represents the estimated proportion of a 2020 source block
#      group's total population located within the overlapping 2010 target block group.
# ==============================================================================

library(tidyverse)
library(tidycensus)
source("R/utils.R")

# ------------------------------------------------------------------------------
# STEP 1: Download 2020 Block Group Total Population Data via Census API
# ------------------------------------------------------------------------------

# Define state list (50 US States + District of Columbia)
states <- c(state.abb, "DC")

# Fetch 2020 Block Group population state-by-state using `purrr::map` to prevent
# API hierarchy length/validation errors in batch queries
pop2020 <- map(states, \(st) {
  get_decennial(
    geography = "block group",
    variables = c(pop_2020 = "P1_001N"), # P1_001N = Total Population in 2020 PL 94-171
    year = 2020,
    sumfile = "pl",
    state = st
  )
}) %>%
  list_rbind() %>% # Row-bind individual state results into a single data frame
  select(
    bg20 = GEOID,   # 12-digit standard Census FIPS block group identifier
    pop_2020 = value # Total population count
  )

# ------------------------------------------------------------------------------
# STEP 2: Read and Process NHGIS 2020-to-2010 Block Group Crosswalk File
# ------------------------------------------------------------------------------

# Read zipped NHGIS crosswalk CSV directly using high-performance `data.table::fread`
xwalk <- data.table::fread("data-raw/source_data/nhgis_bg2020_bg2010.csv.zip") |>

  # Format geographic identifiers as zero-padded 12-digit GEOID strings
  mutate(
    bg20 = str_pad(bg2020ge, width = 12, pad = "0"),
    bg10 = str_pad(bg2010ge, width = 12, pad = "0"),
    st20 = substring(bg20, 1, 2), # Extract 2-digit 2020 State FIPS code
    st10 = substring(bg10, 1, 2)  # Extract 2-digit 2010 State FIPS code
  ) |>

  # Filter out state-boundary anomalies and restrict to US States + DC
  filter(
    st10 == st20,         # Keep only intra-state geographic intersections
    as.integer(st20) <= 56 # Exclude Puerto Rico and outlying territories (FIPS > 56)
  ) |>

  # Retain 2010/2020 GEOIDs and the population allocation proportion
  select(bg10, bg20, wt_pop)

# ------------------------------------------------------------------------------
# STEP 3: Merge Datasets and Compute Intersection Population Weights
# ------------------------------------------------------------------------------

# Combine crosswalk proportions with official 2020 population counts
bg_crosswalk <- inner_join(xwalk, pop2020, by = "bg20") |>

  # Calculate estimated 2020 population for each spatial intersection:
  # (2020 Source Block Group Population) * (Proportion allocated to 2010 Target Block Group)
  mutate(
    xwalk_weight = as.integer(round(pop_2020 * wt_pop))
  ) |>

  # Omit non-overlapping or zero-population spatial intersections
  filter(xwalk_weight > 0) |>

  # Retain final identifiers and calculated weight integer
  select(bg10, bg20, xwalk_weight) |>

  # Sort rows hierarchically for clean internal structure and reproducible diffs
  arrange(bg10, bg20)

# ------------------------------------------------------------------------------
# STEP 4: Export Processed Package Data Object
# ------------------------------------------------------------------------------

# Export crosswalk dataset to package /data directory
use_data2(bg_crosswalk, overwrite = TRUE)

# ------------------------------------------------------------------------------
# Diagnostic Verification:
#   Summing `xwalk_weight` across all intersections should approximate total US
#   2020 Decennial Census population (~331 million residents across 50 states + DC).
#
# sum(bg_crosswalk$xwalk_weight) / 1e6
# ------------------------------------------------------------------------------
