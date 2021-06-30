# PUMA-level socio-demographic and housing variables from ACS 1-year estimates.
# Data for 2012-2019 use the "2010" PUMA codes and definitions ("puma10"
# geographic concordance variable). Variables are restricted to those manually
# identified as natively numeric (e.g. Median number of rooms).

#-----

library(tidycensus)
library(tidyverse)
source("R/utils.R")

#-----

# Use Ummel's Census API key
census_api_key("36db046eb6ef52099da1aef7068ae007e3571220")

# Load and cache the variable table for 2019
vars <- load_variables(year = 2019, dataset = "acs1", cache = TRUE)

#-----

# Years to obtain data for
# Data actually go back to 2005 (see ?get_acs), but it looks like the "2010" version of the PUMA's are introduced in 2012
# So the PUMA codes from 2012-2019 correspond to the "puma10" geographic concordance variable
years <- 2012:2019

#-----

# Table used to manually select 'target.variables'
# temp <- vars %>%
#   filter(grepl("Estimate!!Median", vars$label, fixed = TRUE)) %>%
#   filter(str_sub(label, start = -7, end = -1) == "!!Total" | !grepl("--!!", label, fixed = TRUE)) %>%
#   group_by(label) %>%
#   slice(1L) %>%
#   group_by(concept) %>%
#   slice(1L) %>%
#   arrange(name)
# View(temp)

# Manual list of variable with numeric (median) values
target.variables <- c(
  "B01002_001",
  "B06011_001",
  "B08121_001",
  "B19013_001",
  "B19113_001",
  "B19202_001",
  "B25018_001",
  "B25035_001",
  "B25039_001",
  "B25064_001",
  "B25071_001",
  "B25077_001",
  "B25088_001",
  "B25092_001",
  "B25105_001"
)

#-----

# Download PUMA-level data via Census Bureau API
result <- map_dfr(setNames(years, years),
                  ~ get_acs(geography = "public use microdata area",
                            variable = target.variables,
                            year = .x,
                            survey = "acs1"),
                  .id = "vintage") %>%
  pivot_wider(id_cols = c(GEOID, vintage), names_from = "variable", values_from = "estimate") %>%
  separate(GEOID, into = c("state", "puma10"), sep = 2) %>%
  filter(as.integer(state) %in% 1:56) %>%   # Restrict to 50 states and D.C.
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001)

#-----

# Save result to disk
saveRDS(result, file = "geo-processed/ACS/acs_puma10_2012-2019_processed.rds")
