# MAKE OBSOLETE EVENTUALLY

library(tidyverse)

data("geo_census")
regdiv <- read_csv("other-raw/State codes, regions, and divisions.csv") %>%   # Convert to "geo_*.rda" file eventually
  mutate(state_fips = str_pad(state_fips, width = 2, pad = 0))

#-----

# Labels for the CEX 'popsize' variable
popsize.cats <- c("Less than 100 thousand", "100-500 thousand", "0.5-1.0 million", "1-5 million", "More than 5 million")

# State code, region, division hierarchy

# Census geography hierarchy
temp <- geo_census %>%
  as_tibble() %>%
  filter(POPULATION > 0) %>%
  rename(state_fips = STATEFP, county_fips = COUNTYFP, cbsa_fips = CBSAFP) %>%
  mutate(urban = ifelse(`census | urban`, "urban", "rural")) %>%
  mutate(POPULATION = as.integer(POPULATION))

# Link between official CBSA names and FIPS codes, extracted from most recent census delineation file
# NOTE: URL must be updated manually and should math vintage of CBSA data in 'geo_census'
tmp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1.xls", destfile = tmp)
metro <- readxl::read_excel(tmp, skip = 2) %>%
  filter(!is.na(`FIPS State Code`)) %>%
  rename(cbsa_fips = `CBSA Code`, psu_name = `CBSA Title`) %>%
  select(cbsa_fips, psu_name) %>%
  distinct()
unlink(tmp)

#---

# Retain list of block groups associated with each state-CBSA combination
geoid <- temp %>%
  filter(cbsa_fips != "None") %>%
  group_by(state_fips, cbsa_fips) %>%
  summarize(geoid = list(GEOID), .groups = "drop")

# CBSA population along with state identifier
# NOTE that CBSA's can span state boundaries, so there can be multiple entries for a single CBSA
cbsa <- temp %>%
  filter(cbsa_fips != "None") %>%
  group_by(cbsa_fips) %>%
  mutate(popsize = popsize.cats[findInterval(sum(POPULATION), vec = c(0, 100e3, 500e3, 1e6, 5e6, Inf))],
         psu_type = "CBSA") %>%
  left_join(metro, by = "cbsa_fips") %>%
  group_by(state_fips, cbsa_fips, psu_name, psu_type, popsize, urban) %>%
  summarize(psu_pop = sum(POPULATION), .groups = "drop") %>%
  pivot_wider(names_from = urban, names_prefix = "pop_", values_from = psu_pop, values_fill = list(psu_pop = 0L)) %>%
  left_join(geoid, by = c("state_fips", "cbsa_fips")) %>%  # Add list-column of associated block groups
  rename(psu_fips = cbsa_fips)

# Safety checks
stopifnot(!anyNA(cbsa))
test <- cbsa %>%
  select(state_fips, psu_fips, popsize) %>%
  group_by(state_fips, psu_fips, popsize) %>%
  add_count()
stopifnot(all(test$n == 1))

#---

# Retain list of block groups associated with each state-CBSA combination
geoid <- temp %>%
  filter(cbsa_fips == "None") %>%
  group_by(state_fips, county_fips) %>%
  summarize(geoid = list(GEOID), .groups = 'drop')

# For each state and 'popsize' category, return the county with the largest population outside of CBSA's
noncbsa <- temp %>%
  filter(cbsa_fips == "None") %>%
  group_by(state_fips, county_fips, urban) %>%
  summarize(psu_pop = sum(POPULATION), .groups = 'drop') %>%
  mutate(psu_type = "non-CBSA",
         popsize = "unknown") %>%
  pivot_wider(names_from = urban, names_prefix = "pop_", values_from = psu_pop, values_fill = list(psu_pop = 0L)) %>%
  left_join(geoid, by = c("state_fips", "county_fips")) %>%  # Add list-column of associated block groups
  rename(psu_fips = county_fips)

# Safety check
stopifnot(!anyNA(noncbsa))

#---

psu_info <- bind_rows(cbsa, noncbsa) %>%
  left_join(regdiv, by = "state_fips") %>%
  mutate(psu_name = ifelse(is.na(psu_name), paste0(state_name, " outside CBSA"), psu_name)) %>%
  arrange(region, division, state, psu_name) %>%
  select(names(regdiv), everything())

# Check that the number of block groups assigned to PSU's does not exceed the total number in original 'geo_census' data
stopifnot(length(unlist(psu_info$geoid)) <= nrow(geo_census))

# Save output to disk
save(psu_info, file = "data/psu_info.RData", compress = "bzip2")
