
# Validation exercise for fused RECS-ACS 2015 comparing state-level electricity and natural gas estimates to EIA SEDS totals

#---------

# Obtain and process EIA SEDS state-level residential electricity and natural gas consumption for 2015
# Uses the EIA API via the 'eia' package

# Note: The EIA API calls take some time to execute; once cached, they re-execute quickly within the same session
library(eia)
eia_set_key("irwCitbR7er2Ye262S5gRgNONc5oUS1EyTjOEYlx")

gmerge <- readRDS("geo-raw/miscellaneous/Geographic entities to merge on state.rds")

# Residential electricity, by state
# https://www.eia.gov/opendata/qb.php?category=40559
ecat <- eia_cats(id = "40559")
elec <- map_dfr(ecat$childseries$series_id, eia_series, start = 2015, end = 2015) %>%
  unnest(data) %>%
  transmute(state_postal = substring(geography, 5, 6),
            seds_gwh = value)

# Residential natural gas, by state
# https://www.eia.gov/opendata/qb.php?category=40552
ncat <- eia_cats(id = "40552")
ngas <- map_dfr(ncat$childseries$series_id, eia_series, start = 2015, end = 2015) %>%
  unnest(data) %>%
  transmute(state_postal = substring(geography, 5, 6),
            seds_mcf = value)

# Calculate RECS 2015 total by division and adjust EIA estimates accordingly
data(recs, package = "fusionModel")
rtotal <- recs %>%
  group_by(division) %>%
  summarize(recs_division_gwh = sum(electricity * weight / 1e6),  # GWh electricity
            recs_division_mcf = sum(natural_gas * weight / 1e4),  # Million cubic feet natural gas
            .groups = "drop") %>%
  rename(recs_division = division)

# Combine data and adjust SEDS consumption at division level to match RECS 2015 totals
seds <- inner_join(elec, ngas) %>%
  left_join(gmerge) %>%
  inner_join(rtotal) %>%
  group_by(recs_division) %>%
  mutate(seds_gwh_adj = seds_gwh * recs_division_gwh / sum(seds_gwh),
         seds_mcf_adj = seds_mcf * recs_division_mcf / sum(seds_mcf)) %>%
  ungroup() %>%
  select(state, state_postal, seds_gwh_adj, seds_mcf_adj)

# Confirm adjustments
all.equal(sum(seds$seds_gwh_adj), sum(rtotal$recs_division_gwh))
all.equal(sum(seds$seds_mcf_adj), sum(rtotal$recs_division_mcf))

#---------

# Obtain data inputs on disk used for validation exercise

# ACS 2015 microdata
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst") %>%
  select(acs_2015_hid, state, weight, starts_with("rep_"))

# Fused RECS-ACS 2015 implicates (v1)
sim <- fst::read_fst("production/v1/RECS-ACS 2015/RECS-ACS_2015_v1.fst")

#---------

# Analyze electricity consumption ('kwh'), by state
ex1 <- analyze(formula = kwh ~ 1,
               by = "state",
               synthetic = sim,
               static = acs,
               donor.N = 5686,
               cores = 2)

# Compare to EIA SEDS state residential electricity totals
check <- ex1 %>%
  filter(metric == "total") %>%
  mutate(estimate = estimate / 1e6,
         lower_ci = lower_ci / 1e6,
         upper_ci = upper_ci / 1e6) %>%  # Convert to GWh
  left_join(seds, by = "state") %>%
  mutate(score = 1 - abs(seds_gwh_adj - estimate) / (0.5 * (upper_ci - lower_ci)))

# Check national totals
sum(check$estimate)
sum(check$seds_gwh_adj)

# Relative error of the point estimates
summary(abs(check$estimate - check$seds_gwh_adj) / check$seds_gwh_adj)

# Score indicating how centered the external value is in the estimated CI
# Optimal score = 1; minimum can go negative; zero indicates value at outer limit of 95% CI
summary(check$score)

ggplot(check, aes(x = seds_gwh_adj, y = estimate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(name = "EIA SEDS estimate (GWh in 2015)", breaks = scales::breaks_extended(n = 8)) +
  scale_y_continuous(name = "Fused RECS-ACS estimate (GWh in 2015)", breaks = scales::breaks_extended(n = 8)) +
  theme_bw()

ggplot(check, aes(x = state_postal, y = seds_gwh_adj)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "EIA SEDS estimate compared to RECS-ACS 95% CI (GWh in 2015)", breaks = scales::breaks_extended(n = 8)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#---------

# Analyze natural gas consumption ('cufeetng'), by state
ex2 <- analyze(formula = cufeetng ~ 1,
               by = "state",
               synthetic = sim,
               static = acs,
               donor.N = 5686,
               cores = 2)

# Compare to EIA SEDS state residential natural gas totals
check <- ex2 %>%
  filter(metric == "total") %>%
  mutate(estimate = estimate / 1e4,
         lower_ci = lower_ci / 1e4,
         upper_ci = upper_ci / 1e4) %>%  # Convert to million cubic feet
  left_join(seds, by = "state") %>%
  mutate(score = 1 - abs(seds_mcf_adj - estimate) / (0.5 * (upper_ci - lower_ci)))

# Check national totals
sum(check$estimate)
sum(check$seds_mcf_adj)

# Relative error of the point estimates
summary(abs(check$estimate - check$seds_mcf_adj) / check$seds_mcf_adj)

# Score indicating how centered the external value is in the estimated CI
# Optimal score = 1; minimum can go negative; zero indicates value at outer limit of 95% CI
summary(check$score)

ggplot(check, aes(x = seds_mcf_adj, y = estimate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(name = "EIA SEDS estimate (MMcf in 2015)", breaks = scales::breaks_extended(n = 8)) +
  scale_y_continuous(name = "Fused RECS-ACS estimate (MMcf in 2015)", breaks = scales::breaks_extended(n = 8)) +
  theme_bw()

ggplot(check, aes(x = state_postal, y = seds_mcf_adj)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "EIA SEDS estimate compared to RECS-ACS 95% CI (MMcf in 2015)", breaks = scales::breaks_extended(n = 8)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

