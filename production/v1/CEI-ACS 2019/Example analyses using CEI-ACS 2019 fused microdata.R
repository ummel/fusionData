# The code below provides examples demonstrating how to use the fusionModel
# analyze() function to generate point estimates and standard errors using the
# CEI-ACS 2019 v1 fused microdata. The necessary data files can be downloaded
# at: https://drive.google.com/drive/folders/1hWEtyrtp0VSOkHjBXfttOfOkSf0E0uLE

# Install and load the latest version of the fusionModel package
devtools::install_github("ummel/fusionModel")
library(fusionModel)

# Load CEI-ACS 2019 fused household microdata. This file contains a single
# implicate (synthetic version) of the fused variables. By design, the row order
# within each implicate matches the household row order in 'acs', eliminating
# the need to do a formal join.
syn <- fst::read_fst("CEI-ACS_2019_v1.fst")

# Load subset of ACS household microdata. This is a custom subset of the full
# 2019 ACS that provides necessary weighting variables and a handful of
# additional variables of interest specific to the examples below. In general,
# analyses can make use of any variable observable within the ACS microdata.
acs <- fst::read_fst("ACS_2019_example.fst")

#---------

# The examples below make use of the analyze() function to carry out point
# estimate and standard error pooling across implicates. Please see ?analyze and
# the Examples section therein prior to proceeding. If you are on a non-Windows
# system with multiple cores, you can set the 'cores' argument >1 to reduce
# computation time.

# NOTE: Because 'syn' contains only a single implicate, all calls to analyze()
# below will print a useful warning reminding the user that uncertainty is not
# being calculated across implicates.

# Analyze eating out expenditure ('eatout'), by state
ex1 <- analyze(formula = eatout ~ 1,
               by = "state",
               synthetic = syn,
               static = acs,
               donor.N = 5686)
View(ex1)

# Analyze gasoline expenditure ('gas'), by PUMA
# reference person
ex2 <- analyze(formula = gas ~ 1,
               by = c("state", "puma10"),
               synthetic = syn,
               static = acs,
               donor.N = 5686)
View(ex2)

#---------

# For reference: The code below was used to generate the "ACS_2019_example.fst"
# file from full-resolution ACS microdata within the non-public fusionData
# package.

# Household observations, including weights and select variables
# acs <- fst::read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst") %>%
#   select(state, puma10, weight, starts_with("rep_"))

# Write example data to disk
#fst::write_fst(acs, "production/v1/CEI-ACS 2019/ACS_2019_example.fst")
