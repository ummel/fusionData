
# Rough validation of RECS 2015 electricity and natural gas expenditure when
# fused to ACS 2015 microdata This compares the fused RECS variables to the
# analogous variables native of the ACS, averaged at PUMA-level The ACS utility
# variables are known to have issues, so they are scaled and adjusted below in
# attempt to create something more comparable.

# The validation data are the
# ACS-PUMS "elep" and "gasp" utility expenditure variables. These are
# notoriously noisy and hardly ever used for microdata work. To counter this, I
# excluded households where utilities are included in rent or where there are
# combined utilities on the same bill. Then calculated mean expenditure at the
# PUMA level, adjusted to match SEDS state-level expenditures, then further
# adjusted so the national, per-HH average expenditure matches the RECS 2015
# national average. This is my best attempt at plausible "true" expenditures at
# the PUMA-level -- at least something to compare the fusionACS output to for
# first-pass sanity check.

#-----

# Average electricity expenditure per household in 2015: $1,374
# 1000 * 162.37 / 118.2
# Based on RECS: https://www.eia.gov/consumption/residential/data/2015/c&e/pdf/ce2.6.pdf

# Average natural gas expenditure per household in 2015: $346
# 1000 * 40.89 / 118.2
# Based on RECS: https://www.eia.gov/consumption/residential/data/2015/c&e/pdf/ce2.6.pdf

#-----

library(tidyverse)

# Load fused RECS_2015-ACS_2015 microdata
recs.sim <- fst::read_fst("production/RECS_2015_sim.fst")

#-----

# Load required ACS microdata
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst",
                     columns = c("acs_2015_hid", "state", "puma10", "weight", "elep", "gasp", "elefp", "gasfp"))

#-----

# Load SEDS state-level variables; estimate total state-level residential expenditure for electricity and natural gas
seds <- readRDS("geo-processed/EIA-SEDS/eia-seds_2010-2019_processed.rds") %>%
  filter(vintage == 2015) %>%
  mutate(elec_state_spend_hh = electricity_MWh_hh * electricity_price,
         natgas_state_spend_hh = natgas_price * natgas_tcf_hh) %>%
  select(state, elec_state_spend_hh, natgas_state_spend_hh)

# Estimated PUMA-level electricity and natural gas expenditure (annual)
# This uses only ostensibly "valid" observations of "elep" and "gasp"
# Values adjusted so the state totals resemble SEDS distribution and overall natonal mean matches RECS 2015
# The goal is to get a close to a reasonable PUMA-level estimate as possible, given limitations of the ACS utility expenditures
acs.puma <- acs %>%
  left_join(seds, by = "state") %>%
  mutate(evalid = elefp == "Valid monthly electricity cost in ELEP" & gasfp != "Included in electricity payment",
         gvalid = gasfp %in% c("Valid monthly gas cost in GASP", "No charge or gas not used") & elefp != "No charge or electricity not used",
         elec_state_spend_hh = 1374 * elec_state_spend_hh / weighted.mean(elec_state_spend_hh[evalid], weight[evalid]),
         natgas_state_spend_hh = 346 * natgas_state_spend_hh / weighted.mean(natgas_state_spend_hh[gvalid], weight[gvalid]),
         elep = ifelse(evalid, elep, NA),
         gasp = ifelse(gvalid, gasp, NA)) %>%
  # group_by(state, puma10) %>%
  # mutate(elep = ifelse(elep < quantile(elep, 0.05, na.rm = TRUE) | elep > quantile(elep, 0.95, na.rm = TRUE), NA, elep),  # Clip the 5% extreme values at PUMA level
  #        gasp = ifelse(gasp < quantile(gasp, 0.05, na.rm = TRUE) | gasp > quantile(gasp, 0.95, na.rm = TRUE), NA, gasp)) %>%   # Clip the 5% extreme values at PUMA level
  group_by(state) %>%
  mutate(elep = elep * elec_state_spend_hh / weighted.mean(elep, weight, na.rm = TRUE),
         gasp = gasp * natgas_state_spend_hh / weighted.mean(gasp, weight, na.rm = TRUE),
         totalp = elep + gasp) %>%
  group_by(state, puma10) %>%
  summarize_at(vars(elep, gasp, totalp), ~ weighted.mean(x = .x, w = weight, na.rm = TRUE)) %>%
  ungroup()

#-----

# Compare mean fused utility expenditure, by PUMA, with the ACS-based estimate
comp <- recs.sim %>%
  left_join(acs, by = "acs_2015_hid") %>%
  group_by(state, puma10) %>%
  summarize(puma_weight = sum(weight),  # PUMA total household weight
            total = weighted.mean(dollarel + dollarng, weight),
            dollarel = weighted.mean(dollarel, weight),
            dollarng = weighted.mean(dollarng, weight),
            .groups = "drop") %>%
  left_join(acs.puma, by = c("state", "puma10"))

#-----

# Electricity

# Weighted correlation
cc <- cov.wt(select(comp, dollarel, elep), wt = comp$puma_weight, cor = TRUE)$cor[2, 1]

# Weighted R-squared
r2 <- summary(lm(comp$dollarel ~ comp$elep, weights = comp$puma_weight))$r.squared

# Root mean squared error
rmse <- sqrt(weighted.mean((comp$dollarel - comp$elep) ^ 2, w = comp$puma_weight))

# Hexbin plot
g <- ggplot(comp, aes(x = elep, y = dollarel)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, color = 2, linetype = "dashed") +
  scale_x_continuous("Plausible 'true' mean electricity expenditure, by PUMA (2015)", labels = scales::dollar, limits = c(0, 2500)) +
  scale_y_continuous("Fused RECS-ACS mean electricity expenditure, by PUMA (2015)", labels = scales::dollar, limits = c(0, 2500)) +
  annotate("text", x = 0, y = 2500, hjust = 0, label = paste0("Correlation: ", round(cc, 2))) +
  annotate("text", x = 0, y = 2400, hjust = 0, label = paste0("R-squared: ", round(r2, 2))) +
  annotate("text", x = 0, y = 2300, hjust = 0, label = paste0("RMSE: $", round(rmse))) +
  theme_bw(12)

g

#-----

# Natural gas

# Weighted correlation
cc <- cov.wt(select(comp, dollarng, gasp), wt = comp$puma_weight, cor = TRUE)$cor[2, 1]  # Weighted correlation

# Weighted R-squared
r2 <- summary(lm(comp$dollarng ~ comp$gasp, weights = comp$puma_weight))$r.squared

# Root mean squared error
rmse <- sqrt(weighted.mean((comp$dollarng - comp$gasp) ^ 2, w = comp$puma_weight))

# Hexbin plot
g <- ggplot(comp, aes(x = gasp, y = dollarng)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, color = 2, linetype = "dashed") +
  scale_x_continuous("Plausible 'true' mean natural gas expenditure, by PUMA (2015)", labels = scales::dollar, limits = c(0, 1200)) +
  scale_y_continuous("Fused RECS-ACS mean natural gas expenditure, by PUMA (2015)", labels = scales::dollar, limits = c(0, 1200)) +
  annotate("text", x = 0, y = 1200, hjust = 0, label = paste0("Correlation: ", round(cc, 2))) +
  annotate("text", x = 0, y = 1150, hjust = 0, label = paste0("R-squared: ", round(r2, 2))) +
  annotate("text", x = 0, y = 1100, hjust = 0, label = paste0("RMSE: $", round(rmse))) +
  theme_bw(12)

g
