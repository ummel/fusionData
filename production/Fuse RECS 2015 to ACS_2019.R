library(fusionData)
library(fusionModel)
source("R/utils.R")

#-----

# Prepare and assemble data inputs
# !!! Retain only small subset of potential fusion variables; this subset will be used for chained fusion of CEI
# Could do a full fusion and then utilize only the desired fusion variables, but this is faster for simply demonstrating chained survey fusion process

prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2019",
                respondent = "household",
                implicates = 5)

data <- assemble(prep,
                 fusion.variables = c("cooltype", "dollarel", "dollarng", "dollarlp", "dollarfo"),
                 window = 2)

rm(prep)
gc()

#-----

# Train fusion models (10 sec)

fit <- train(data = data$RECS_2015,
             y = attr(data, "fusion.vars"),
             ignore = "recs_2015_hid",
             weight = "weight",
             cores = 3,
             complexity = 1e-5)

#-----

# Fuse variables (30 sec)
# Reduce precision of numeric fused variables for smaller file size
# Add ACS 2019 household ID column

sim <- fuse(data = data$ACS_2019, train.object = fit) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  cbind(data$ACS_2019["acs_2019_hid"]) %>%
  select(acs_2019_hid, everything())

stopifnot(!anyNA(sim))

#-----

# Example: save result to disk (about 10 MB)
#fst::write_fst(sim, "production/RECS_2019_sim.fst", compress = 100)
