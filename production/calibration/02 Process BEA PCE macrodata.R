# Desired vintage of the macrodata
year <- "2019"

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

#---

#   # National PCE
#   npce <- BEA_pce_national %>%
#   select(pce_series, !!year) %>%
#   rename(national_pce = !!year) %>%
#   right_join(cat_assignment, by = "pce_series") %>%
#   group_by(cat) %>%
#   summarize(national_pce = sum(national_pce * national_pce_adj))
#
# # National aggregates
# # If the initial PUMS estimate is higher, it is retained as the "true" value
# nagg <- npce %>%
#   left_join(custom.aggregates, by = "cat") %>%
#   mutate(nat_agg = ifelse(is.na(custom), national_pce, custom)) %>%
#   select(cat, nat_agg)

#stopifnot(nrow(nagg) == length(unique(cat_assignment$cat)))

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

# TURNED OFF FOR NOW...
# State EIA residential fuel totals ($millions)

# eagg <- eia_state %>%
#   filter(year == !!year, !is.na(state_fips)) %>%
#   pivot_longer(cols = ELEC:OFUEL, names_to = "cat", values_to = "eia_agg") %>%
#   select(cat, state_fips, eia_agg) %>%
#   rename(state = state_fips)

#-----

# National PCE, state PCE, and state EIA (fuels) aggregates
# Static data.table prepared in advance
# Note: the reassignment of 'major' to create separate "Gasoline" and "Vehicles" entries is designed to let 'major' better reflect differences in likelihood of underreporting
#  The assignment is relevant when imputing adjustment factors for categories without aggregates (in the calibration process)
# cat_aggregates <- sagg %>%
#   left_join(nagg) %>%
#   #left_join(eagg) %>%
#   left_join(cat_assignment %>% select(major, cat, category) %>% distinct()) %>%
#   # mutate(major = ifelse(cat == "GAS", "Gasoline", major),
#   #        major = ifelse(major == "Transportation" & grepl("vehicle", tolower(category)), "Vehicles", major),
#   #        major = ifelse(major %in% c("Gasoline", "Vehicles", "Housing", "Utilities and phone"), major, "Other")) %>%
#   select(-category) %>%
#   data.table(key = c("state", "cat"))
#
# #-----
#
# # Save output to disk
# save(cat_aggregates, file = "data/cat_aggregates.RData")

