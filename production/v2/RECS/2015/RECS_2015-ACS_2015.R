library(fusionData)
library(fusionModel)

#-----

# Prepare and assemble data inputs
prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 5)

# Removed pca for prep 
data <- assemble(prep,
                 fusion.variables = c("btung", "kwh", "cooltype","scalee","scaleg",'scaleb','noheatng', "btufo", "btulp",
                                      "noacbroke","noacel","noheatel",'noheatbroke','noheatbulk'),
                 window = 2)

# Number of training observations; used by analyze()
N <- nrow(data$RECS_2015)

#-----
#Create new custom variabes 
data$RECS_2015 <- data$RECS_2015 %>%
  mutate(
    insec = scalee != "Never" | scaleg != "Never"| scaleb != "Never",
    noheat = noheatbroke == "Yes" | noheatbulk == "Yes" |  noheatel == "Yes" | noheatng == "Yes",
    noac = noacel == "Yes" | noacbroke == "Yes"
  ) %>%
  select(-all_of(c("scalee","scaleb",'scaleg', "noacel", "noacbroke", "noheatbroke", "noheatbulk", "noheatel", "noheatng")))

# Clean up to clear memory
rm(prep)
gc()

#---------


# Sanity check
# lapply(data, dim)
#
# # Visual check of frequencies for a harmonized variable
# round(table(data$RECS_2015$nhsldmem__np) / nrow(data[[1]]), 3)
# round(table(data$ACS_2015$nhsldmem__np) / nrow(data[[2]]), 3)

#-----
# Select desired fusion variables
fusion.vars <- c("kwh","btung","btufo","btulp","cooltype","insec", "noheat", "noac")

# Predictor variables
# This is just a fancy way to pull the predictor variables from 'data' attributes
pred.vars <- unlist(map(c("harmonized.vars", "location.vars", "spatial.vars"), ~ attr(data, .x)))

# Get fusion sequence and blocking
# Can sample 'data' if training dataset is too large/slow
fsequence <- blockchain(data = data$RECS_2015,
                        y = fusion.vars,
                        x = pred.vars,
                        weight = "weight",
                        cores = 1)  #Have to change this for Windows/Mac

#-----

# Train fusion model
# re: threads, see: https://github.com/ummel/fusionModel/issues/26
train(data = data$RECS_2015,
      y = fsequence,
      x = pred.vars,
      file = "production/v2/RECS/2015/RECS_2015.fsn",
      weight = "weight",
      nfolds = 5,
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
# Optimal settings for 'k' and 'max_dist' are unknown at moment -- using default values
# See here re: multi-threading: https://github.com/ummel/fusionModel/issues/26
sim <- fuse(data = data$ACS_2015,
             file = "production/v2/RECS/2015/RECS_2015.fsn",
             k = 5,
             M = 100)  # Using 100 implicates for consistency with v1 deliverable 

# Save simulation results to disk
fst::write_fst(x = sim,
               path = "production/v2/RECS/2015/RECS_2015-ACS_2015.fst",
               compress = 100)

#-----
# Save simulation results to disk
test <- fst::read_fst(
               path = "production/v2/RECS/2015/RECS_2015-ACS_2015.fst")
gc()
