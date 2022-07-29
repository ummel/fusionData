library(fusionModel)

#-----

# Load the training data
train.data <- read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_train.fst")

# Extract variable names from the prediction data (without loading to memory)
pred.vars <- names(fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_predict.fst"))

# Identify the fusion variables
fusion.vars <- setdiff(names(train.data), c("weight", pred.vars))

# Number of spatial implicates in 'train.data'
nsimp <- 5

# 'nsimp' can also be auto-detected but preferable to hard-code based on knowledge of the *_input.R script
# nsimp <- train.data[fusion.vars] %>%
#   map(~ rle(.x)$lengths) %>%
#   unlist() %>%
#   min()

#-----

# Identify fusion sequence and blocking strategy
fchain <- blockchain(data = train.data,
                     y = fusion.vars,
                     x = pred.vars,
                     delta = 0.01,
                     maxsize = 3,
                     weight = "weight",
                     nfolds = 5,
                     fraction = min(1, 10e3 * nsimp / nrow(train.data)),
                     cores = 3)

#-----

# Train fusion model
fsn.path <- train(data = train.data,
                  y = fchain,
                  x = pred.vars,
                  file = "fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_model.fsn",
                  weight = "weight",
                  nfolds = 0.75,
                  cores = 3,
                  hyper = list(boosting = "goss",
                               num_leaves = 2 ^ (5) - 1,
                               min_data_in_leaf = unique(round(pmax(20, nrow(train.data) / nsimp * 0.001 * c(1)))),
                               feature_fraction = 0.8,
                               num_iterations = 1000,
                               learning_rate = 0.05)
)

#----

# Fuse multiple implicates to training data for internal validation analysis
# Note that 'data' is limited to the first spatial implicate in 'train.data'
valid <- fuseM(data = train.data[seq(nrow(train.data) / nsimp), ],
               file = fsn.path,
               k = 10,
               M = 50,
               ignore_self = TRUE,
               cores = 3)

# Save 'valid' as .fst
fst::write_fst(x = valid, path = "fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_valid.fst", compress = 100)

# Clean up
rm(train.data, valid)

#----

# Load the prediction data
pred.data <- read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_predict.fst")

# Fuse multiple implicates to ACS
# Optimal settings for 'k' and 'max_dist' are unknown at moment -- using default values
sim <- fuseM(data = pred.data,
             file = fsn.path,
             k = 10,
             M = 50,
             cores = 3)

# Save 'sim' as .fst
fst::write_fst(x = sim, path = "fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_fused.fst", compress = 100)

# Clean up
rm(pred.data, sim)
