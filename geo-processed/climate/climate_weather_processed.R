library(tidyverse)

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
  mutate(
    state = substring(StCoFIPS, 1, 2),
    county10 = substring(StCoFIPS, 3, 5),
    year = data.table::year(Date)
  ) %>%
  mutate(
    across(WBGTmin_C:WBGTmean_C, ~ fif(Flag_WBGT > 1, NA_real_, .x)),
    across(UTCImean_C, ~ fif(Flag_UTCI > 1, NA_real_, .x))
  ) %>%
  select(state, county10, year, WBGTmin_C:WBGTmean_C, UTCImean_C)

#---

# Linear relationship between WBGT and UTCI
#fit <- lm(WBGTmean_C ~ UTCImean_C, data = slice_sample(d, prop = 0.1))

# Here we estimate the equivalent WBGT value that will serve as reference temperature for degree days calculations
# The midpoint of the UTCI thermal comfort zone is 17.5C (9C-26C)
# However, this assumes a reference person walking outside
# Assume walking vs. sitting increases perceived temperature by ~2.5C (so take 20C at the comfortable, indoor UTCI temperature)
#predict(fit, data.frame(UTCImean_C = 20))

# The adjusted WBGT reference temperature is very close to the standard 65F/18.33C base temperature for degree days
wbgt.ref <- 18.333

#---

# For the District of Columbia (FIPS 11001), simply replicate the data for Arlington County, VA (FIPS 51013)
dc <- d %>%
  filter(state == "51", county10 == "013") %>%
  mutate(state = "11", county10 = "001")

# Annual, county-level, WBGT-based degree days relative to 'wbgt.ref' reference temperature
degreedays <- d %>%
  bind_rows(dc) %>%  # Add data for Washington D.C.
  mutate(dw = 0.5 * WBGTmin_C + 0.5 * WBGTmax_C - wbgt.ref) %>%
  group_by(state, county10, year) %>%
  summarize(hdd_wbgt = fif(sum(is.na(dw)) / length(dw) > 0.5, NA_real_, -1 * sum(pmin(dw, 0))),
            cdd_wbgt = fif(sum(is.na(dw)) / length(dw) > 0.5, NA_real_, sum(pmax(dw, 0))),
            .groups = "drop") %>%
  mutate(across(hdd_wbgt:cdd_wbgt, ~ .x * 9 / 5))  # Converts degree days from C to F for consistency with ClimDiv and station degree days

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
cdd <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-cddccy-v1.0.0-20231106")  # CDD
hdd <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-hddccy-v1.0.0-20231106")  # HDD
precip <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpncy-v1.0.0-20231106")  # Precipitation

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

# All county FIPS codes
counties <- fst::read_fst("geo-processed/concordance/geo_concordance.fst") %>%
  select(state, county10, recs_iecc_zone) %>%
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
    left_join(counties %>% filter(state == state_fips) %>% select(-recs_iecc_zone),
              by = join_by(state),
              relationship = "many-to-many") %>%
    na.omit()
}

# Data for Hawaii (Honolulu)
# Use weather station data for Honolulu Airport for all counties in Hawaii
hi <- processStationData("geo-raw/heatmetrics/Honolulu Weather Station Data.csv", "15")

# Data for Alaska (Anchorage)
# Use weather station data for Anchorage Airport for all counties in Alaska
ak <- processStationData("geo-raw/heatmetrics/Anchorage Weather Station Data.csv", "02")

#---

# Check for any HI or AK data in ClimDiv
if (any(climdiv$state %in% c("02", "15"))) stop("Looks like there is HI and/or AK data in ClimDiv...")

# Combine ClimDiv with HI and AK station data, then merge Spangler et al. WBGT degree days
# Restricted to 1999 and later, since Anchorage station data only complete back to 1999
out <- climdiv %>%
  bind_rows(hi) %>%
  bind_rows(ak) %>%
  left_join(degreedays, by = join_by(state, county10, year)) %>%
  inner_join(counties, by = join_by(state, county10)) %>%
  filter(year >= 1999)  # Alaska station data goes back to 1999

# Linear regression models to impute WBGT degree days using conventional/ambient degree days, precip, and climate zone
fit.hdd <- lm(hdd_wbgt ~ hdd + hdd:recs_iecc_zone + precip:recs_iecc_zone + year:recs_iecc_zone, data = out)
fit.cdd <- lm(cdd_wbgt ~ cdd + cdd:recs_iecc_zone + precip:recs_iecc_zone + year:recs_iecc_zone, data = out)

# Impute missing WBGT degree days and cleanup
weather <- out %>%
  mutate(hdd_wbgt = fif(is.na(hdd_wbgt), pmax(0, predict(fit.hdd, .)), hdd_wbgt),
         cdd_wbgt = fif(is.na(cdd_wbgt), pmax(0, predict(fit.cdd, .)), cdd_wbgt)) %>%
  arrange(state, county10, year) %>%
  rename(vintage = year) %>%
  select(-recs_iecc_zone)

# Long-term mean values (climate "normals")
normals <- weather %>%
  group_by(state, county10) %>%
  summarize(across(cdd:cdd_wbgt, mean, .names = "{.col}_normal"), .groups = "drop") %>%
  mutate(vintage = "always") %>%
  select(state, county10, vintage, everything())

# Check correlations
# cor(weather$cdd, weather$cdd_wbgt)
# cor(weather$hdd, weather$hdd_wbgt)

#---

# Save weather and climate normals results to disk
saveRDS(weather, "geo-processed/climate/weather_1999-2022_processed.rds")
saveRDS(normals, "geo-processed/climate/climate_normals_processed.rds")
