# Tract-level home price index from Federal Housing Finance Agency
# https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx

# Note that the FHFA HPI is NOT adjusted for inflation
# https://www.fhfa.gov/Media/PublicAffairs/Pages/House-Price-Index-Frequently-Asked-Questions.aspx#quest17

library(tidyverse)
# source("R/utils.R")
# source("R/utils_geo.R")
# load("R/sysdata.rda")

data(cpi_series)

#-------------------------------

# # All census tracts
# tracts <- fst::read_fst("geo-processed/concordance/geo_concordance.fst") %>%
#   select(state, county10, tract10) %>%
#   distinct() %>%
#   mutate(tract = paste0(state, county10, tract10))

# Link to .csv file is hard-coded
# File is updated over time
# Original source: https://www.fhfa.gov/data/hpi/datasets?tab=additional-data
data.url <- "https://www.fhfa.gov/hpi/download/annually/hpi_at_bdl_tract.csv"

# Download the full dataset and adjust the 'hpi' variable for inflation
# The number of annual tract-level observations appears to be high and stable from 2003 onward
fhfa <- data.url %>%
  read_csv(na = c("", "NA", ".")) %>%
  filter(!is.na(hpi), year >= 2003)

# Most recent year in dataset
max.year <- max(fhfa$year, na.rm = TRUE)

# Create final output
fhfa <- fhfa %>%
  left_join(cpi_series, by = join_by(year)) %>%
  mutate(state = substring(tract, 1, 2),
         county10 = substring(tract, 3, 5),
         tract10 = substring(tract, 6, 11),
         hpi = hpi * cpi) %>% # Adjust for inflation
  group_by(state, county10, tract10) %>%
  mutate(ok = max(year) == max.year & n() >= ceiling(0.75 * (max.year - 2003))) %>% # Restrict to tracts with data for year = 'max.year' and sufficient data points
  filter(ok) %>%
  mutate(hpi_real = hpi / hpi[year == max.year], # Set HPI = 100 for most recent year
         hpi_relmax = hpi / max(hpi), # Current year HPI relative to maximum over period
         hpi_relmin = hpi / min(hpi), # Current year HPI relative to minimum over period
         hpi_cov = sd(hpi) / mean(hpi), # HPI coefficient of variation over period (time-invariant)
         hpi_relchg = diff(range(hpi)) / min(hpi)) %>%   # Relative degree of change between min and max HPI over period (time-invariant)
  ungroup() %>%
  select(state, county10, tract10, year, hpi_real:hpi_relchg) %>%
  rename(vintage = year) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001)

saveRDS(fhfa, "geo-processed/FHFA/FHFA_processed.rds")
