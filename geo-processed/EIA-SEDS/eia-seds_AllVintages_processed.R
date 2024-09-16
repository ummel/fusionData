# State Energy Data System (SEDS)
# https://www.eia.gov/state/seds/
# Extract and process annual, state-wide residential energy consumption estimates and prices, by fuel

library(tidyverse)
source("R/utils.R")

# Crosswalk between state FIPS and postal codes
state.xwalk <- "geo-processed/concordance/geo_concordance.fst" %>%
  fst::read_fst(columns = c("state", "state_postal")) %>%
  distinct()

# The first year to process; will include all subsequent years in the current SEDS database
first_year <- 2010

#--------------------------------------------

# Consolidated SEDS data files: https://www.eia.gov/state/seds/seds-data-complete.php?sid=US#CompleteDataFile
# NOTE: There are two versions of the "consolidated" SEDS data files available:
# 1: https://www.eia.gov/state/seds/seds-data-complete.php contains the complete, final dataset
# 2: https://www.eia.gov/state/seds/seds-data-fuel.php contains partially-complete data for the most-recent year
# I use #2 below, since it always contains the most-recent data, even if incomplete

# Get current SEDS "Codes and Descriptions" file (Excel)
tmp <- tempfile()
download.file(url = "https://www.eia.gov/state/seds/CDF/Codes_and_Descriptions.xlsx", destfile = tmp)
codes <- readxl::read_excel(path = tmp, sheet = "MSN descriptions", skip = 10)
unlink(tmp)

# Get the current SEDS "Consolidated data file" (.zip)
tmp <- tempfile()
#download.file(url = "https://www.eia.gov/state/seds/CDF/Complete_SEDS.zip", destfile = tmp) # Version 1 (see note above)
download.file(url = "https://www.eia.gov/state/seds/sep_update/Complete_SEDS_update.zip", destfile = tmp) # Version 2 (see note above)

# Unzip and process raw SEDS data
# Retains only select variables
seds <- read.csv(unz(tmp, filename = "complete_seds_update.csv"), stringsAsFactors = FALSE) %>%
  left_join(codes, by = "MSN") %>%
  #filter(MSN %in% c("MGACD", "ESRCP", "ESRCD", "NGRCP", "NGRCD", "LGRCP", "LGRCD", "DFRCP", "DFRCD")) %>%
  filter(MSN %in% c("MGTXD", "ESRCP", "ESRCD", "NGRCP", "NGRCD", "PQRCP", "PQRCD", "KSRCP", "DFRCP", "DFRCD", "ESRCV", "NGRCV", "PQRCV", "DFRCV", "KSRCV", "WDRCV")) %>%  # Codes switched from LPG to Propane in 2016
  mutate(series = paste(Description, Unit, sep = " | ")) %>%
  select(StateCode, Year, series, Data) %>%
  filter(Year >= first_year) %>%
  spread(series, Data) %>%
  rename(state_postal = StateCode, year = Year) %>%
  inner_join(state.xwalk, by = "state_postal")

# Remove temporary directory
unlink(tmp)

#-----

# The number of households in each state-year based on 1-year ACS survey
# see ?get_acs: ACS data only available back to 2010 via that function (though help file says 2009)
# NOTE: Due to COVID, there are no 1-year ACS estimates for 2020. The code below interpolates a household count for 2020.
all.years <- 2010:max(seds$year)
acs.years <- setdiff(all.years, 2020)
household.count <- acs.years %>%
  map(~ tidycensus::get_acs(geography = "state", variables = "B11001_001", state = NULL, year = .x, survey = "acs1")) %>%
  setNames(acs.years) %>%
  bind_rows(.id = "year") %>%
  rename(state = GEOID, hh = estimate) %>%
  select(year, state, hh) %>%
  mutate(year = factor(year, levels = all.years)) %>%
  complete(year, state) %>%
  group_by(state) %>%
  mutate(hh = as.integer(approx(x = year, y = hh, xout = year)$y),
         year = as.integer(as.character(year))) %>%
  ungroup()

#-----

# Merge SEDS, household count, and (optionally) CPI index
# TURNED OFF: Note that fuel price variables are adjusted for inflation over time and indexed to the CPI in year: max(household.count$year)
temp <- seds %>%
  inner_join(household.count, by = c("state", "year")) %>%
  #left_join(cpi.index.df %>% mutate(cpi = cpi / cpi[year == max(household.count$year)]), by = "year") %>%  # For inflation adjustment
  # Average fuel price variables
  mutate(gasoline_price = (120.476 / 1000) * `Motor gasoline average price, all end-use sectors | Dollars per million Btu`,  # Convert gasoline price to dollars per gallon
         electricity_price = `Electricity price in the residential sector | Dollars per million Btu`,
         natgas_price = `Natural gas price in the residential sector (including supplemental gaseous fuels) | Dollars per million Btu`,
         lpg_price = `Propane price in the residential sector | Dollars per million Btu`,
         fueloil_price = `Distillate fuel oil price in the residential sector | Dollars per million Btu`) %>%

  # Total residential expenditure variables
  mutate(electricity_expend = `Electricity expenditures in the residential sector | Million dollars`,
         natgas_expend = `Natural gas expenditures in the residential sector (including supplemental gaseous fuels) | Million dollars`,
         lpg_expend = `Propane expenditures in the residential sector | Million dollars`,
         fueloil_expend = `Distillate fuel oil expenditures in the residential sector | Million dollars` + `Kerosene expenditures in the residential sector | Million dollars`) %>%

  #mutate_at(vars(ends_with("_price")), funs(cpi * .)) %>%    # Adjust fuel prices for inflation
  mutate_at(vars(ends_with("_price")), na_if, 0) %>%    # Replace any zero prices with NA
  arrange(state, year)

#-----

# Compute consumption-per-household for residential fuels
# Also retains the '*_price' variables in output
seds.state <- temp %>%
  filter(state != "US") %>%
  mutate(
    # Average consumption of all households, whether users of the fuel or not
    electricity_MWh_hh = 1000 * `Electricity consumed by (sales to ultimate customers in) the residential sector | Million kilowatthours` / hh,
    natgas_tcf_hh = 1000 * `Natural gas delivered to the residential sector, used as consumption (including supplemental gaseous fuels) | Million cubic feet` / hh,
    lpg_gal_hh = 42 * 1000 * `Propane consumed by the residential sector | Thousand barrels` / hh,
    fueloil_gal_hh = 42 * 1000 * (`Distillate fuel oil consumed by the residential sector | Thousand barrels` + `Kerosene consumed by the residential sector | Thousand barrels`) / hh
  ) %>%
  mutate(
    # Average expenditure of all households, whether users of the fuel or not
    electricity_expend_hh = 1e6 * electricity_expend / hh,
    natgas_expend_hh = 1e6 * natgas_expend / hh,
    lpg_expend_hh = 1e6 * lpg_expend / hh,
    fueloil_expend_hh = 1e6 * fueloil_expend / hh
  ) %>%
  select(year, state, gasoline_price:fueloil_price, electricity_MWh_hh:fueloil_gal_hh, electricity_expend:fueloil_expend, electricity_expend_hh:fueloil_expend_hh)

#-----

# Impute any missing values
# Linear interpolation/extrapolation, by state
seds.state <- seds.state %>%
  arrange(year) %>%
  group_by(state) %>%
  mutate_at(vars(contains("_")), ~ approx(1:length(.), ., xout = 1:length(.), rule = 2)$y) %>%
  ungroup()

stopifnot(!anyNA(seds.state))

#-----

# Assemble final result
result <- seds.state %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  select_if(~ novary(.x) == FALSE) %>%  # Remove variables without variation
  as_tibble() %>%
  rename(vintage = year) %>%
  select(state, vintage, everything())

# Save result to disk
#saveRDS(result, paste0("geo-processed/EIA-SEDS/eia-seds_", paste(range(result$vintage), collapse = "-"), "_processed.rds"))
saveRDS(result, "geo-processed/EIA-SEDS/eia-seds_AllVintages_processed.rds")
