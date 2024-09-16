library(tidyverse)
source("R/utils.R")

# All census tracts
tracts <- fst::read_fst("geo-processed/concordance/geo_concordance.fst") %>%
  select(state, county10, tract10) %>%
  distinct()

# Original dataset contains UHI estimates (and related variables) for all urban census tracts in the U.S. (includes all states)
# Retains the UHI (Urban Heat Island) variables
# Note that rural tract UHI values are set to zero on advice of dataset creator: Chakraborty, TC <tirthankar.chakraborty@pnnl.gov>
uhi <- read_csv("geo-raw/UHI/Census_UHI_US_Urbanized_recalculated.csv", show_col_types = FALSE) %>%
  mutate(Census_geoid = str_pad(Census_geoid, width = 11, pad = 0),
         state = substring(Census_geoid, 1, 2),
         county10 = substring(Census_geoid, 3, 5),
         tract10 = substring(Census_geoid, 6, 11)) %>%
  inner_join(tracts, by = join_by(state, county10, tract10)) %>%  # Drops tracts not present in the geo concordance file
  right_join(tracts, by = join_by(state, county10, tract10)) %>%  # Add all census tracts not in the original UHI dataset (should be rural tracts)
  mutate(vintage = "always", # Since UHI unlikely to change quickly over time, assume it is constant over time
         across(UHI_annual_day:UHI_winter_night, ~ replace_na(.x, 0))) %>%  # Replace all missing (i.e. rural) UHI values with zero value on advice of dataset creator
  select(state, county10, tract10, vintage, UHI_annual_day:UHI_winter_night) %>%
  mutate_if(is.numeric, cleanNumeric, tol = 0.001)

saveRDS(uhi, "geo-processed/UHI/UHI_processed.rds")
