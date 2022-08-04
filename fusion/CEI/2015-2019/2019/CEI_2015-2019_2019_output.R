library(fusionModel)

local.run = FALSE

if(local.run){
  # Local settings
  in.dir = out.dir = "fusion/CEI/2015-2019/2019/"
  num.cores = 3
}else{
  # Savio settings
  scratch.dir = "/global/scratch/users/ckingdon/"
  in.dir = file.path(scratch.dir, "input_fusionACS/CEI_2019/")
  out.dir = file.path(scratch.dir, "output_fusionACS/CEI_2019/")
  num.cores = as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
}

#------------------------------------------------------------------------------

# Turn off data.table and fst multithreading to prevent forking issues
setDTthreads(1)
threads_fst(1)

#------------------ Prep the inputs --------------------------------------------

# Load the training data
train.data <- read_fst(file.path(in.dir, "CEI_2015-2019_2019_train.fst"))

# Extract variable names from the prediction data (without loading to memory)
pred.vars <- names(fst(file.path(in.dir, "CEI_2015-2019_2019_predict.fst")))

# Identify the fusion variables
fusion.vars <- setdiff(names(train.data), c("weight", pred.vars))

# Number of spatial implicates in 'train.data'
nsimp <- 5

# 'nsimp' can also be auto-detected but preferable to hard-code based on knowledge of the *_input.R script
# nsimp <- train.data[fusion.vars] %>%
#   map(~ rle(.x)$lengths) %>%
#   unlist() %>%
#   min()

# Rows in 'train.data' for just the first spatial implicate
fsimp <- seq(to = nrow(train.data), by = nsimp)

# Initial production run: restrict train.data to first spatial implicate
# Need to test more with multiple implicates
train.data <- train.data[fsimp, ]

#------------------ Blockchain -------------------------------------------------

# Identify fusion sequence and blocking strategy
# Note that 'data' is limited to the first spatial implicate in 'train.data'
start = Sys.time()
fchain <- blockchain(data = train.data,
                     y = fusion.vars,
                     x = pred.vars,
                     maxsize = 1,  # Blocking is expensive with semi-continuous variables, so turned off
                     weight = "weight",
                     nfolds = 5,
                     fraction = min(1, 50e3  / length(fsimp)),
                     cores = num.cores)
print(Sys.time() - start)

#------------------ Train ------------------------------------------------------

# Train fusion model
start = Sys.time()
fsn.path <- train(data = train.data,
                  y = fchain,
                  x = pred.vars,
                  file = file.path(out.dir, "CEI_2015-2019_2019_model.fsn"),
                  weight = "weight",
                  nfolds = 0.75,
                  fork = TRUE,
                  cores = num.cores,
                  hyper = list(boosting = "goss",
                               num_leaves = 2 ^ (5) - 1,
                               min_data_in_leaf = unique(round(pmax(10, length(fsimp) * 0.0005 * c(1)))),
                               feature_fraction = 0.8,
                               num_iterations = 1000,
                               learning_rate = 0.05)
)
print(Sys.time() - start)

# Once train() is complete, reset number of threads allowed in data.table and fst
setDTthreads(num.cores)
threads_fst(num.cores)

#------------------ Fuse - validation ------------------------------------------

# Fuse multiple implicates to training data for internal validation analysis
start = Sys.time()
valid <- fuse(data = train.data,
              file = fsn.path,
              k = 10,
              M = 30,
              ignore_self = TRUE,
              cores = num.cores)
print(Sys.time() - start)

# Save 'valid' as .fst
fst::write_fst(x = valid, path = file.path(out.dir, "CEI_2015-2019_2019_valid.fst"), compress = 100)

# Clean up
rm(train.data, valid)

#------------------ Fuse - simulation ------------------------------------------

# Load the prediction data
pred.data <- read_fst(file.path(in.dir, "CEI_2015-2019_2019_predict.fst"))

# Fuse multiple implicates to ACS
fsn.path = file.path(out.dir, "CEI_2015-2019_2019_model.fsn")
start = Sys.time()
sim <- fuse(data = pred.data,
            file = fsn.path,
            k = 10,
            M = 30,
            cores = num.cores)
print(Sys.time() - start)

# Save 'sim' as .fst
fst::write_fst(x = sim, path = file.path(out.dir, "CEI_2015-2019_2019_fused.fst"), compress = 100)

# Clean up
rm(pred.data, sim)