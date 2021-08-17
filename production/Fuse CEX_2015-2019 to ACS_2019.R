library(fusionData)
library(fusionModel)
source("R/utils.R")

#-----

# Prepare and assemble data inputs

prep <- prepare(donor = "CEI_2015-2019",
                recipient = "ACS_2019",
                respondent = "household",
                implicates = 5)

data <- assemble(prep,
                 window = 2)

# Add custom variable "mortint_share"
temp <- read_fst("survey-processed/CEX/CEI/CEI_2015-2019_H_processed.fst") %>%
  mutate(mortgage = mrtgip + mrtgpp,
         mortint_share = ifelse(mortgage == 0, 0, mrtgip / mortgage)) %>%
  select(cei_hid, mortint_share)

data$`CEI_2015-2019` <- data$`CEI_2015-2019` %>%
  left_join(temp, by = "cei_hid")

rm(prep, temp)
gc()

#-----

# Sanity check
lapply(data, dim)

# Visual check of frequencies for a harmonized variable
round(table(data$`CEI_2015-2019`$bedroomq__bdsp) / nrow(data[[1]]), 3)
round(table(data$ACS_2019$bedroomq__bdsp) / nrow(data[[2]]), 3)

#-----

# In this example, there are (at least) 5 conceptually-similar variables common to the RECS and CEI (that are not already harmonized to the ACS in some way)
# They are: "cntralac", "windowac", "elec", "ngas", "ofuel"
# The strategy here is to use the 5 "shared" variables as predictors in the CEI fusion models
# The analogous variables from the RECS are then merged to the ACS recipient data (using result of prior RECS_2015 to ACS_2019 fusion)
# The recipient data then has all of the necessary predictor variables to call fuse() using the train() object fitted to CEI microdata

# Prepare the donor
# The numeric variables are converted to percentiles, as is typically done for harmonized predictor variables
data$`CEI_2015-2019` <- data$`CEI_2015-2019` %>%
  mutate(elec = convertPercentile(elec, weight),
         ngas = convertPercentile(ngas, weight),
         ofuel = convertPercentile(ofuel, weight))

# Prepare the recipient
# Merge pre-fused RECS "2019" variables to harmonized ACS 2019 microdata
# Manually construct the 5 shared predictors from RECS variables so that they have same name, values, class as the CEI variables
recs.2019 <- read_fst("production/RECS_2019_sim.fst")
data$ACS_2019 <- data$ACS_2019 %>%
  left_join(recs.2019, by = "acs_2019_hid") %>%
  mutate(cntralac = factor(ifelse(grepl("central", cooltype), "Yes", "No")),
         windowac = factor(ifelse(grepl("individual", cooltype), "Yes", "No")),
         elec = convertPercentile(dollarel, weight),
         ngas = convertPercentile(dollarng, weight),
         ofuel = convertPercentile(dollarfo + dollarlp, weight)) %>%
  select(-any_of(names(recs.2019)[-1]))  # Drop the original RECS variables (but retain household ID)

#-----

# Select fusion variables from among the full potential set
# Excludes variables shared with RECS
# Include the custom "mortint_share" variable
fvars <- setdiff(attr(data, "fusion.vars"), c("cntralac", "windowac", "elec", "ngas", "ofuel"))
fvars <- c(fvars, "mortint_share")

# Variables in CEI microdata that we'd rather just ignore entirely
# These are variables that probably don't need to be in the microdata at all, or could be harmonized with ACS with some additional effort, etc.
drop <- c("building", "hh_cu_q", "hlfbathq", "incnonw1", "incomey1", "no_earnr", "swimpool", "fmlpyyrx")

#-----

# Train fusion models (25 min)

fit <- train(data = data$`CEI_2015-2019`,
             y = setdiff(fvars, drop),
             ignore = c("cei_hid", drop),
             weight = "weight",
             cores = 3,
             complexity = 1e-5)

#-----

# Fuse variables (18 min)
sim <- fuse(data = data$ACS_2019, train.object = fit)

# Add primary mortgage interest and principal expenditure variables manually
# Remaining numeric variables can be converted to integer (either expenditure or financial balances variables)
# Also adds ACS 2019 household ID column
temp <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst", columns = c("acs_2019_hid", "mortgage"))
stopifnot(all(temp$acs_2019_hid == data$ACS_2019$acs_2019_hid))
sim <- sim %>%
  cbind(temp) %>%
  mutate(
    mrtgip = round(mortint_share * mortgage),
    mrtgpp = mortgage - mrtgip,
  ) %>%
  select(-mortgage, -mortint_share) %>%
  mutate_if(is.numeric, ~ as.integer(round(.x))) %>%
  select(acs_2019_hid, everything())

stopifnot(!anyNA(sim))

#-----

# Example: save result to disk (71 MB)
fst::write_fst(sim, "production/CEX_2015-2019_sim.fst", compress = 100)
