# Program to randomly assign counties to PUMAs so as to produce county level estimates of fused variables
# Adapted from Karthik's program: Random assignment of counties to ACS 2015 households
# HA 7/31/23

#rm(list = ls())
#options(scipen=999)
library(pacman)
p_load(here, tidyverse, data.table)
setwd(here())

# # Desired number of implicates
# M <- 30
# yr <- 2019

# Load ACS microdata
acs0 <- fst::read_fst(paste0("survey-processed/ACS/", yr, "/ACS_", yr, "_H_processed.fst"), columns = c(paste0("acs_", yr, "_hid"), "weight", "state", "puma10")) %>%
  rename(HID = eval(paste0("acs_", yr, "_hid")))

# Number of housing units (puma_weight) in each PUMA-county intersection
geo <- fst::read_fst("geo-processed/concordance/geo_concordance.fst", columns = c("state", "puma10", "county10", "puma_weight")) %>%
  group_by(state, puma10, county10) %>%
  summarise(puma_weight = sum(puma_weight))

# Random assignment of counties to ACS households across M implicates
county.rnd <- geo %>%
  left_join(dplyr::count(acs0, state, puma10)) %>%  # Merge in number of ACS observations for each PUMA; column 'n' by default
  group_by(state, puma10) %>%
  summarise(county10 = list(as.data.frame(replicate(n = M, sample(county10, size = n[1], prob = puma_weight, replace = TRUE)))), .groups = "drop") %>%
  unnest(cols = county10) %>%
  mutate(HID = acs0$HID[order(acs0$state, acs0$puma10)]) %>%
  arrange(HID)

# Safety check
all(county.rnd$HID == acs0$HID)

names(county.rnd) <- c(setdiff(names(county.rnd), "HID"), paste0("acs_", yr, "_hid"))

# Ancillary data: estimated number of households in each county
# This is estimated as the county's mean total weight across implicates
county.hh <- county.rnd %>%
  mutate(weight = acs0$weight) %>%
  pivot_longer(cols = starts_with("V"), names_to = "M", values_to = "county10") %>%
  group_by(state, county10, M) %>%
  summarise(total = sum(weight)) %>%
  summarise(hh = mean(total)) %>%
  ungroup()
#fst::write_fst(county.hh,path= "production/validation/weights_bycounty.fst")

rm(geo, acs0)
gc()
