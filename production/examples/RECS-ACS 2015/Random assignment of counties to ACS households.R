# Desired number of implicates
M <- 10

# Load ACS microdata
acs0 <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst", columns = c("acs_2015_hid", "weight", "state", "puma10"))

# Number of housing units (puma_weight) in each PUMA-county intersection
geo <- fst::read_fst("geo-processed/concordance/geo_concordance.fst", columns = c("state", "puma10", "county10", "puma_weight")) %>%
  group_by(state, puma10, county10) %>%
  summarize(puma_weight = sum(puma_weight))

# Random assignment of counties to ACS households across M implicates
county.rnd <- geo %>%
  left_join(count(acs0, state, puma10)) %>%  # Merge in number of ACS observations for each PUMA; column 'n' by default
  group_by(state, puma10) %>%
  summarize(county10 = list(as.data.frame(replicate(n = M, sample(county10, size = n[1], prob = puma_weight, replace = TRUE)))), .groups = "drop") %>%
  unnest(cols = county10) %>%
  mutate(acs_2015_hid = acs0$acs_2015_hid[order(acs0$state, acs0$puma10)]) %>%
  arrange(acs_2015_hid)

# Safety check
all(county.rnd$acs_2015_hid == acs0$acs_2015_hid)

# Ancillary data: estimated number of households in each county
# This is estimated as the county's mean total weight across implicates
county.hh <- county.rnd %>%
  mutate(weight = acs0$weight) %>%
  pivot_longer(cols = starts_with("V"), names_to = "M", values_to = "county10") %>%
  group_by(state, county10, M) %>%
  summarize(total = sum(weight)) %>%
  summarize(hh = mean(total)) %>%
  ungroup()

rm(geo, acs0)
gc()
