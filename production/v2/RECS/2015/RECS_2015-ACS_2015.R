library(fusionData)
library(fusionModel)

#-----

# Specify the variables to be fused
# I've chosen the variables specified here, excluding energy expenditures
# https://docs.google.com/document/d/1YlgrP7hRe13skQtHUzyY5eqx-6_F_pZ5nzmYnkgF13k/edit?pli=1
fusion.vars <- c("totsqft_en", "cufeetng", "kwh", "cooltype", "scalee", "noacbroke", "noacel",
                 "noacdays", "noheatbroke", "noheatel", "noheatng")

#-----

# Prepare and assemble data inputs

# Single implicate for testing (ideally more for production)
prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 1)

# Don't need to use pce option; just using for speed-up during testing
data <- assemble(prep,
                 fusion.variables = fusion.vars,
                 pca = c(25, 0.95),
                 window = 2)

# Clean up to clear memory
rm(prep)
gc()

#-----

# Sanity check
# lapply(data, dim)
#
# # Visual check of frequencies for a harmonized variable
# round(table(data$RECS_2015$nhsldmem__np) / nrow(data[[1]]), 3)
# round(table(data$ACS_2015$nhsldmem__np) / nrow(data[[2]]), 3)

#-----

# Predictor variables
# This is just a fancy way to pull the predictor variables from 'data' attributes
pred.vars <- unlist(map(c("harmonized.vars", "location.vars", "spatial.vars"), ~ attr(data, .x)))

# Get fusion sequence and blocking
# Can sample 'data' if training dataset is too large/slow
fsequence <- blockchain(data = data$RECS_2015,
                        y = fusion.vars,
                        x = pred.vars,
                        weight = "weight",
                        cores = 3)

#-----

# Train fusion model
# Using nfolds = 0.75 for testing; could set to 5 for production
# re: threads, see: https://github.com/ummel/fusionModel/issues/26
train(data = data$RECS_2015,
      y = fsequence,
      x = pred.vars,
      file = "production/v2/RECS/2015/RECS_2015.fsn",
      weight = "weight",
      nfolds = 0.75,
      threads = 3,
      hyper = list(boosting = "gbdt",
                   feature_fraction = 0.8,
                   num_iterations = 1000,
                   learning_rate = 0.05)
)

#-----

# Test fusion with ACS using a single implicate
# test <- fuse(data = data$ACS_2015,
#              file = "production/v2/RECS/2015/RECS_2015.fsn",
#              k = 100,
#              max_dist = 0.05)
#
# # Sanity check
# table(test$cooltype) / nrow(test)
# table(data$RECS_2015$cooltype) / nrow(data$RECS_2015)

#-----

# Fuse variables to ACS for multiple implicates
# Optimal settings for 'k' and 'max_dist' are unknown at moment -- using plausible values
# See here re: multi-threading: https://github.com/ummel/fusionModel/issues/26
sim <- fuseM(data = data$ACS_2015,
             file = "production/v2/RECS/2015/RECS_2015.fsn",
             k = 100,
             max_dist = 0.05,
             M = 2)  # Set higher for production

# Save simulation results to disk
fst::write_fst(x = sim,
               path = "production/v2/RECS/2015/RECS_2015-ACS_2015.fst",
               compress = 100)
