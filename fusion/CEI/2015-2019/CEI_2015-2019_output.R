library(fusionModel)

#-----

# Load the training data
train.data <- read_fst("fusion/CEI/2015-2019/CEI_2015-2019_train.fst")

# Extract variable names from the prediction data (without loading to memory)
pred.vars <- names(fst("fusion/CEI/2015-2019/CEI_2015-2019_predict.fst"))

# Identify the fusion variables
fusion.vars <- setdiff(names(train.data), c("weight", pred.vars))

#-----

# Get fusion sequence and blocking
fchain <- blockchain(data = train.data,
                     y = fusion.vars,
                     x = pred.vars,
                     delta = 0.01,
                     maxsize = 3,
                     weight = "weight",
                     nfolds = 5,
                     fraction = min(1, 50e3 / nrow(train.data)),
                     cores = 3)

#-----

# Train fusion model
fsn.path <- train(data = train.data,
                  y = fchain,
                  x = pred.vars,
                  file = "fusion/CEI/2015-2019/CEI_2015-2019_model.fsn",
                  weight = "weight",
                  nfolds = 5,
                  cores = 3,
                  hyper = list(boosting = "goss",
                               num_leaves = 2 ^ (4:6) - 1,
                               min_data_in_leaf = unique(round(pmax(20, nrow(train.data) * 0.0001 * c(1, 5)))),
                               feature_fraction = 0.8,
                               num_iterations = 5000,
                               learning_rate = 0.05)
)

#----

# Remove the training data
rm(train.data)

# Load the prediction data
pred.data <- read_fst("fusion/CEI/2015-2019/CEI_2015-2019_predict.fst")

# Fuse multiple implicates to ACS
# Optimal settings for 'k' and 'max_dist' are unknown at moment -- using default values
sim <- fuseM(data = pred.data,
             file = fsn.path,
             k = 10,
             M = 50,
             cores = 3)

# Save result as .fst
fst::write_fst(x = sim,
               path = "fusion/CEI/2015-2019/CEI_2015-2019_fused.fst",
               compress = 100)

# Cleanup
#rm(sim, pred.data)
