library(tidyverse)
source("R/utils.R")

# Fast if/else drop-in used in code below
fif <- data.table::fifelse

#---

# Load the raw daily data (2000-2020) from Spangler et al. (2022)
# NOTE: This was only downloaded temporarily, because the file was very large
d <- readRDS("geo-raw/climate/Heatvars_County_2000-2020_v1.2.rds")

# Create desired time and space identifying variables
# Set observations to NA when the associated data quality flag is > 1
# Flag value "0" means the estimate is based on data representing ≥50% of the population; "1" covers 10–49% of the population
d <- d %>%
  mutate(state = substring(StCoFIPS, 1, 2),
         county10 = substring(StCoFIPS, 3, 5),
         year = data.table::year(Date),
         across(UTCImin_C:UTCImean_C, ~ fif(Flag_UTCI > 1, NA_real_, .x))) %>%
  select(state, county10, year, UTCImin_C:UTCImean_C)

#---

# For the District of Columbia (FIPS 11001), simply replicate the data for Arlington County, VA (FIPS 51013)
dc <- d %>%
  filter(state == "51", county10 == "013") %>%
  mutate(state = "11", county10 = "001")

# Annual, county-level, UTCI-based degree days relative to 20C reference UTCI temperature
# The midpoint of the UTCI thermal comfort zone is 17.5C (9C-26C)
# See here: https://www.researchgate.net/figure/UTCI-assessment-scale-with-comfort-stress-categories_fig2_361809767
# However, this assumes a reference person walking outside
# Assume walking vs. sitting increases perceived temperature by ~2.5C (so take 20C as the comfortable, stationary/indoor reference UTCI temperature) for calculation of degree days
degreedays <- d %>%
  bind_rows(dc) %>%  # Add data for Washington D.C.
  mutate(dw = UTCImean_C - 20) %>%
  group_by(state, county10, year) %>%
  summarize(hdd_utci = fif(sum(is.na(dw)) / length(dw) > 0.5, NA_real_, -1 * sum(pmin(dw, 0))),
            cdd_utci = fif(sum(is.na(dw)) / length(dw) > 0.5, NA_real_, sum(pmax(dw, 0))),
            .groups = "drop") %>%
  mutate(across(hdd_utci:cdd_utci, ~ .x * 9 / 5))  # Converts degree days from C to F for consistency with ClimDiv and station degree days

# Annual, county-level, number of days where max/min temperature exceed thermal comfort extremes
# See here: https://www.researchgate.net/figure/UTCI-assessment-scale-with-comfort-stress-categories_fig2_361809767
# "Strong heat stress" begins at UTCI 32C
# "Very strong heat stress" begins at UTCI 38C
# "Strong cold stress" begins at UTCI -27C
# "Very strong cold stress" begins at UTCI -40C
stress <- d %>%
  bind_rows(dc) %>%  # Add data for Washington D.C.
  group_by(state, county10, year) %>%
  summarize(strongheat_utci = sum(UTCImax_C >= 32),
            veryheat_utci = sum(UTCImax_C >= 38),
            strongcold_utci = sum(UTCImin_C <= -27),
            verycold_utci = sum(UTCImin_C <= -40),
            .groups = "drop")

#-----

# Download data from NOAA Monthly U.S. Climate Divisional Database (NClimDiv)
# https://data.noaa.gov/onestop/collections/details/f418d493-80c1-4d33-9188-53c6a7451f4e
# There are climate division ("dv"), county (cy), and state ("st") data files

# NClimDiv state code lookup
state.codes <- readRDS("geo-raw/miscellaneous/Geographic entities to merge on state.rds") %>%
  select(state, nclimdiv_code)

# County degree days (base 65F) and precipitation
# Data download (updated regularly): https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
# Data dictionary: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/county-readme.txt
# Note that the url ending date updates regularly...
cdd <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-cddccy-v1.0.0-20240906")  # CDD
hdd <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-hddccy-v1.0.0-20240906")  # HDD
precip <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpncy-v1.0.0-20240906")  # Precipitation

climdiv <- rbind(cdd, hdd, precip) %>%
  mutate(V1 = str_pad(V1, width = 11, pad = 0),
         across(V2:V13, ~ fif(.x < 0, NA_real_, .x)),  # Set negative values (e.g. -9999) to NA
         nclimdiv_code = substring(V1, 1, 2),
         county10 = substring(V1, 3, 5),
         year = as.integer(substring(V1, 8, 11)),
         type = case_match(substring(V1, 6, 7),
                           "01" ~ "precip",
                           "25" ~ "hdd",
                           "26" ~ "cdd")) %>%
  mutate(total = rowSums(.[paste0("V", 2:13)])) %>%   # Total CDD, HDD, precip across all 12 months
  left_join(state.codes, by = join_by(nclimdiv_code)) %>%
  select(state, county10, year, type, total) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  na.omit()  # Remove if there is not complete data

#---

# Process weather station data for Hawaii (Honolulu) and Alaska (Anchorage)

# All county FIPS codes with climate zones
counties <- readRDS("geo-processed/climate/climate_zones_processed.rds") %>%
  select(state, county10, iecc_zone) %>%
  mutate(iecc_zone = fif(iecc_zone == 8, "7", iecc_zone)) %>%
  distinct()

# Function to process weather station data file obtained from: https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
processStationData <- function(file_csv, state_fips) {
  read_csv(file_csv, show_col_types = FALSE) %>%
    mutate(state = state_fips,
           year = data.table::year(DATE),
           month = data.table::month(DATE),
           dt = 0.5 * TMIN + 0.5 * TMAX - 65) %>%
    group_by(year, month) %>%
    mutate(adj = fif(sum(is.finite(dt)) < 10, NA_real_, pmax(1, (365 / 12) / sum(is.finite(dt))))) %>%
    group_by(state, year) %>%
    mutate(N = length(unique(month[is.finite(adj)]))) %>%
    summarize(precip = ifelse(N[1] < 12, NA, sum(PRCP * adj, na.rm = T)),
              hdd = ifelse(N[1] < 12, NA, -1 * sum(pmin(dt, 0) * adj, na.rm = T)),
              cdd = ifelse(N[1] < 12, NA, sum(pmax(dt, 0) * adj, na.rm = T)),
              .groups = "drop") %>%
    left_join(counties %>% filter(state == state_fips) %>% select(-iecc_zone),
              by = join_by(state),
              relationship = "many-to-many") %>%
    na.omit()
}

# Data for Hawaii (Honolulu)
# Use weather station data for Honolulu Airport for all counties in Hawaii
hi <- processStationData("geo-raw/climate/Honolulu Weather Station Data.csv", "15")

# Data for Alaska (Anchorage)
# Use weather station data for Anchorage Airport for all counties in Alaska
ak <- processStationData("geo-raw/climate/Anchorage Weather Station Data.csv", "02")

#---

# Check for any HI or AK data in ClimDiv
if (any(climdiv$state %in% c("02", "15"))) stop("Looks like there is HI and/or AK data in ClimDiv...")

# Combine ClimDiv with HI and AK station data, then merge Spangler et al. degree days and stress metrics
# Restricted to 1999 and later, since Anchorage station data only complete back to 1999
out <- climdiv %>%
  bind_rows(hi) %>%
  bind_rows(ak) %>%
  inner_join(counties, by = join_by(state, county10)) %>%
  left_join(degreedays, by = join_by(state, county10, year)) %>%
  left_join(stress, by = join_by(state, county10, year)) %>%
  filter(year >= 1999)  # Alaska station data goes back to 1999

# Linear regression models to impute degree days using conventional/ambient degree days, precip, and climate zone
fit.hdd <- lm(hdd_utci ~ hdd + hdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)
fit.cdd <- lm(cdd_utci ~ cdd + cdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)
fit.strongheat <- lm(strongheat_utci ~ hdd + hdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)
fit.veryheat <- lm(veryheat_utci ~ hdd + hdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)
fit.strongcold <- lm(strongcold_utci ~ cdd + cdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)
fit.verycold <- lm(verycold_utci ~ cdd + cdd:iecc_zone + precip:iecc_zone + year:iecc_zone, data = out)

# Impute missing WBGT degree days and cleanup
weather <- out %>%
  mutate(
    hdd_utci = fif(is.na(hdd_utci), pmax(0, predict(fit.hdd, .)), hdd_utci),
    cdd_utci = fif(is.na(cdd_utci), pmax(0, predict(fit.cdd, .)), cdd_utci),
    strongheat_utci = fif(is.na(strongheat_utci), pmin(pmax(0, predict(fit.strongheat, .)), 365), strongheat_utci),
    veryheat_utci = fif(is.na(veryheat_utci), pmin(pmax(0, predict(fit.veryheat, .)), 365), veryheat_utci),
    strongcold_utci = fif(is.na(strongcold_utci), pmin(pmax(0, predict(fit.strongcold, .)), 365), strongcold_utci),
    verycold_utci = fif(is.na(verycold_utci), pmin(pmax(0, predict(fit.verycold, .)), 365), verycold_utci)
  ) %>%
  arrange(state, county10, year) %>%
  rename(vintage = year) %>%
  select(-iecc_zone) %>%
  mutate_if(is.numeric, cleanNumeric, tol = 0.001)

# Long-term mean values (climate normals)
wvars <- setdiff(names(weather), c('state', 'county10', 'vintage'))
climate <- weather %>%
  group_by(state, county10) %>%
  summarize(across(all_of(wvars), mean, .names = "{.col}_normal"), .groups = "drop") %>%
  mutate(vintage = "always") %>%
  select(state, county10, vintage, everything()) %>%
  mutate_if(is.numeric, cleanNumeric, tol = 0.001)

# Check correlations
# cor(weather$cdd, weather$cdd_utci)
# cor(weather$hdd, weather$hdd_utci)

#---

# Save weather and climate normals results to disk
saveRDS(weather, "geo-processed/climate/weather_processed.rds")
saveRDS(climate, "geo-processed/climate/climate_processed.rds")
