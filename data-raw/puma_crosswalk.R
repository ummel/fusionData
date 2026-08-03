# ==============================================================================
# DATA PREPARATION: 2010 to 2020 PUMA Crosswalk (puma_crosswalk)
# ==============================================================================
# Objective:
#   Construct a clean mapping table between 2010 and 2020 Census Public Use
#   Microdata Areas (PUMAs) with population-based intersection weights.
#
# Source Data Details:
#   - Source: IPUMS USA / NHGIS 2010-to-2020 PUMA Crosswalk
#   - URL: https://usa.ipums.org/usa/volii/pumas20.shtml
#   - Primary Weight Column (`Part_Pop20`): Represents the estimated total
#     2020 population residing within the geographic intersection of a
#     specific 2010 PUMA and 2020 PUMA.
# ==============================================================================

library(tidyverse)
source("R/utils.R")

# 1. Read raw crosswalk Excel file and clean/harmonize geography records
puma_crosswalk <- readxl::read_excel("data-raw/source_data/PUMA2010_PUMA2020_crosswalk.xls", sheet = 1) |>

  # Filter out edge-case boundary anomalies across state lines and exclude non-state territories
  filter(
    State10 == State20,         # Restrict to intersections in same state (small number of observations violate this in source)
    as.integer(State20) <= 56   # Restrict to 50 US States + DC (FIPS codes <= 56, excluding PR and outlying territories)
  ) |>

  # Standardize geographic identifier variable names to lower_snake_case
  rename(
    state  = State20,
    puma10 = PUMA10,
    puma20 = PUMA20
  ) |>

  # Cast intersection population count to an integer to serve as the crosswalk weight
  mutate(
    xwalk_weight = as.integer(Part_Pop20) # Estimated 2020 population of intersection area between 2010 and 2020 PUMAs
  ) |>

  # Drop zero-population intersections (non-overlapping or unpopulated spatial boundaries)
  filter(xwalk_weight > 0) |>

  # Retain only required geographic identifiers and weight column
  select(state, puma10, puma20, xwalk_weight) |>

  # Sort rows hierarchically for consistent data frame ordering and reproducible diffs
  arrange(state, puma10, puma20)

# 2. Write processed data object to the package's /data directory
use_data2(puma_crosswalk, overwrite = TRUE)

# ------------------------------------------------------------------------------
# Diagnostic Verification:
#   Summing `xwalk_weight` across all intersections should match the total US 2020
#   Decennial Census population (~331 million residents across 50 states + DC).
#
#   sum(puma_crosswalk$xwalk_weight) / 1e6
# ------------------------------------------------------------------------------
