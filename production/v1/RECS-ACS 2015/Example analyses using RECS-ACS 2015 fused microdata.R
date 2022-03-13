# The code below provides examples demonstrating how to use the fusionModel
# analyze() function to generate point estimates and standard errors using the
# RECS-ACS 2015 v1 fused microdata. The necessary data files can be downloaded
# at: https://drive.google.com/drive/folders/1Oz-u8QqPikrExD8hFg2rAlCDc_U0_6jn

# Install and load the latest version of the fusionModel package
devtools::install_github("ummel/fusionModel")
library(fusionModel)

# Load RECS-ACS 2015 fused household microdata. This file contains 10 implicates
# (synthetic versions) of the fused variables, indicated by the "M" (implicate)
# column. By design, the row order within each implicate matches the household
# row order in 'acs', eliminating the need to do a formal join.
syn <- fst::read_fst("RECS-ACS_2015_v1.fst")

# Load subset of ACS household microdata. This is a custom subset of the full
# 2015 ACS that provides necessary weighting variables and a handful of
# additional variables of interest specific to the examples below. In general,
# analyses can make use of any variable observable within the ACS microdata.
acs <- fst::read_fst("ACS_2015_example.fst")

#---------

# The examples below make use of the analyze() function to carry out point
# estimate and standard error pooling across implicates. Please see ?analyze and
# the Examples section therein prior to proceeding. If you are on a non-Windows
# system with multiple cores, you can set the 'cores' argument >1 to reduce
# computation time.

# Analyze electricity consumption ('kwh'), by state
ex1 <- analyze(formula = kwh ~ 1,
               by = "state",
               synthetic = syn,
               static = acs,
               donor.N = 5686)
View(ex1)

# Analyze probability of service disconnect ('scalee'), by state and race of
# reference person
ex2 <- analyze(formula = scalee ~ 1,
               by = c("state", "race"),
               synthetic = syn,
               static = acs,
               donor.N = 5686)
View(ex2)

# Analyze probability of forgoing of heating or cooling in last year due to lack
# of money (noac | noheat), by state and presence of person under 18 in household
ex3 <- analyze(formula = went_without ~ 1,
               by = c("state", "r18"),
               synthetic = mutate(syn, went_without = noac | noheat),  # Create a custom variable combining 'noac' and 'noheat'
               static = acs,
               donor.N = 5686)
View(ex3)

# Analyze square footage ('totsqft_en'), by PUMA
ex4 <- analyze(formula = totsqft_en ~ 1,
               by = c("state", "puma10"),
               synthetic = syn,
               static = acs,
               donor.N = 5686)
View(ex4)

#---------

# For reference: The code below was used to generate the "ACS_2015_example.fst"
# file from full-resolution ACS microdata within the non-public fusionData
# package.

# Person observations. Retain race of reference person, derived from original variables 'rac1p' and 'hisp'
# acs.p <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_P_processed.fst", columns = c("acs_2015_hid", "sporder", "rac1p", "hisp")) %>%
#   filter(sporder == 1) %>%
#   mutate(race = ifelse(rac1p %in% c("White alone", "Black or African American alone"), as.character(rac1p), "Other"),
#          race = ifelse(hisp == "Not Spanish / Hispanic / Latino", race, "Latino"),
#          race = factor(race)) %>%
#   select(acs_2015_hid, race)

# Household observations, including weights and select variables, with 'race' merged from person file
# acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst") %>%
#   left_join(acs.p, by = "acs_2015_hid") %>%
#   select(acs_2015_hid, state, puma10, weight, r18, race, starts_with("rep_"))

# Write example data to disk
#fst::write_fst(acs, "production/v1/RECS-ACS 2015/ACS_2015_example.fst")
