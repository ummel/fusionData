library(fusionData)
library(fusionModel)

#---------

# Data prep
prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 5)

# Data assembly for select RECS variables
data <- assemble(prep,
                 fusion.variables = c("kwh", "cufeetng", "totsqft_en", "cooltype", "scalee", "noacel", "noacbroke", "noheatbroke", "noheatbulk", "noheatel", "noheatng"),
                 spatial.datasets = "all")

# Create custom fusion variables
# This combines some of the energy security questions into aggregate indicators
# Example: Custom variable 'disconnect' is TRUE if household experienced any service disconnect in previous 12 months
data$RECS_2015 <- data$RECS_2015 %>%
  mutate(
    disconnect = scalee != "Never",
    noheat = noheatbroke == "Yes" | noheatbulk == "Yes" |  noheatel == "Yes" | noheatng == "Yes",
    noac = noacel == "Yes" | noacbroke == "Yes"
  ) %>%
  select(-all_of(c("scalee", "noacel", "noacbroke", "noheatbroke", "noheatbulk", "noheatel", "noheatng")))

rm(prep)
gc()

#---------

# Select desired fusion variables
#fusion.vars <- c("kwh", "cufeetng", "totsqft_en", "cooltype", "scalee", "noheat", "noac")
fusion.vars <- c("kwh", "disconnect", "noheat", "noac")

# Train fusion model
fit <- train(data = data$RECS_2015,
             y = fusion.vars,
             x = setdiff(names(data$ACS_2015), c("acs_2015_hid", "weight")),
             weight = "weight",
             lasso = 1,
             complexity = 1e-4,
             cores = 3)

#---------

# Generate 10 implicates
# This is memory intensive, so setting cores = 1
sim <- fuseM(data = data$ACS_2015, train.object = fit, M = 10, cores = 1)

# Example: Save simulation results to disk
# fst::write_fst(x = rbindlist(lapply(sim, as.data.table), idcol = "M"),
#                path = "production/RECS_2015_H-ACS_2015_H.fst",
#                compress = 100)

#---------

# Load desired ACS variables and weights

# Person observations. Retain race of reference person, derived from original variables 'rac1p' and 'hisp'
acs.p <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_P_processed.fst", columns = c("acs_2015_hid", "sporder", "rac1p", "hisp")) %>%
  filter(sporder == 1) %>%
  mutate(race = ifelse(rac1p %in% c("White alone", "Black or African American alone"), as.character(rac1p), "Other"),
         race = ifelse(hisp == "Not Spanish / Hispanic / Latino", race, "Latino"),
         race = factor(race)) %>%
  select(acs_2015_hid, race)

# Household observations, including weights, with 'race' merged from person file
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst") %>%
  select(acs_2015_hid, state, weight, starts_with("rep_")) %>%
  left_join(acs.p, by = "acs_2015_hid")

rm(acs.p)
gc()

#---------

# Example analyses

# To derive estimates at county-level, we randomly assign counties to ACS households based on known PUMA
# The script below generates the random county assignments across 10 implicates
source("production/examples/RECS-ACS 2015/Random assignment of counties to ACS households.R")
all(acs$acs_2015_hid == county.rnd$acs_2015_hid)  # Safety check to confirm row order is OK

# Modify 'sim' by adding random county assignment from 'county.rnd' for each implicate
# This effectively treats county assignment as a simulated variable that varies across implicates
# The advantage is that uncertainty in county assignment is implicitly captured by analyze()
sim <- lapply(1:length(sim), function(i) mutate(sim[[i]], county10 = county.rnd[[paste0("V", i)]]))

# Analyze electricity consumption ('kwh'), by county
ex1 <- analyze(formula = kwh ~ 1,
               by = c("state", "county10"),
               synthetic = sim,
               static = acs,
               donor.N = 5686,
               cores = 2)

# Analyze service disconnect indicator ('disconnect'), by state and race
ex2 <- analyze(formula = disconnect ~ 1,
               by = c("state", "race"),
               synthetic = sim,
               static = acs,
               donor.N = 5686,
               cores = 2)
