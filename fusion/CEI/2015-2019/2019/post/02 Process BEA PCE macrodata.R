# This script generates and saves 3 outputs to disk:
# National PCE calibration targets.rds
# State PCE calibration targets.rds
# State EIA calibration targets.rds

#-----

library(fusionData)
load("data/token.rda")
googlesheets4::gs4_auth(token = token)

#-----

# Desired vintage of the macrodata
year <- "2019"

# Load static inputs
load("survey-processed/CEX/cat_assignment.rda")
data(cpi_series)
data(BEA_pce_national)
data(BEA_pce_state)

#-----

# Load custom aggregates, adjust for inflation, and convert to $millions for consistency with PCE units
custom.aggregates <- googlesheets4::read_sheet(ss = "13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg", sheet = "Custom Aggregates", skip = 1) %>%
  left_join(cpi_series %>%
              group_by(year) %>%
              summarize(cpi = mean(cpi)) %>%
              mutate(cpi = cpi[year == !!year] / cpi),
            by = "year") %>%
  group_by(aggregate) %>%
  summarize(custom = round(sum(billions * cpi * 1e3))) %>%
  rename(cat = aggregate)

#-----

# National PCE
nagg <- BEA_pce_national %>%
  select(pce_series, !!year) %>%
  rename(national_pce = !!year) %>%
  rbind(custom.aggregates, use.names = FALSE) %>%
  right_join(cat_assignment, by = "pce_series") %>%
  group_by(cat) %>%
  summarize(total = sum(national_pce * national_pce_adj)) %>%
  filter(!is.na(total)) %>%
  mutate(cat = as.list(tolower(cat)))

#-----

# State aggregate totals ($millions)

# State PCE
spce <- BEA_pce_state %>%
  rename(state = state_fips,
         state_series = pce_series,
         state_pce = !!year) %>%
  right_join(cat_assignment, by = "state_series") %>%
  complete(state, cat) %>%
  select(state, cat, state_series, state_pce) %>%
  filter(!is.na(state)) %>%
  distinct()

# For each state PCE series, what is the ratio of total (national) value to total value in 'spce'
# national_state_comp <- npce %>%
#   left_join(cat_assignment, by = "cat") %>%
#   distinct(cat, national_pce, state_series) %>%
#   group_by(state_series) %>%
#   summarize(national_pce = sum(national_pce)) %>%
#   inner_join(spce %>% distinct(state, state_series, state_pce) %>% group_by(state_series) %>% summarize(state_pce = sum(state_pce)) %>% na.omit(), by = "state_series") %>%
#   mutate(ratio = national_pce / state_pce)

# Adjust 'spce' so that 'state_pce' better reflects the amount of consumption captured by each 'state_series'
# spce <- spce %>%
#   left_join(national_state_comp %>% select(state_series, ratio), by = "state_series") %>%
#   mutate(state_pce = state_pce * ratio) %>%
#   select(-ratio)

#(nrow(spce) / 51 == nrow(npce))

# State aggregates (no custom aggregates to add currently)
sagg <- spce %>%
  mutate(total = state_pce) %>%
  select(cat, state, state_series, total) %>%
  filter(!is.na(total)) %>%
  group_by(state, total) %>%
  summarize(cat = list(tolower(cat)), .groups = "drop")

#-----

# State EIA residential fuel totals ($millions)
eagg <- readRDS("geo-processed/EIA-SEDS/eia-seds_2010-2019_processed.rds") %>%
  filter(vintage == 2019) %>%
  select(state, ends_with("_expend")) %>%
  mutate(ofuel = lpg_expend + fueloil_expend) %>%
  rename(elec = electricity_expend, ngas = natgas_expend) %>%
  select(state, elec, ngas, ofuel) %>%
  pivot_longer(cols = -state, names_to = "cat", values_to = "total") %>%
  mutate(cat = as.list(cat))

#-----

# Save results to disk
saveRDS(nagg, "fusion/CEI/2015-2019/2019/post/National PCE calibration targets.rds")
saveRDS(sagg, "fusion/CEI/2015-2019/2019/post/State PCE calibration targets.rds")
saveRDS(eagg, "fusion/CEI/2015-2019/2019/post/State EIA calibration targets.rds")
