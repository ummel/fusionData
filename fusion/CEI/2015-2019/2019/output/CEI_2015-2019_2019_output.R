library(fusionModel)

# Directory in /fusion where input files are located
dir <- "fusion/CEI/2015-2019/2019/input"

# Number of cores to use
ncores <- 3

# Number of implicates to generate
nimps <- 40

# Output files path stub
out.path <- file.path(sub("input", "output", dir), sub("train.fst$", "", list.files(dir, "train\\.fst$")))

#-----

# Deprecated code but useful for server runs

# Local settings
# in.dir <- out.dir <- "fusion/RECS/2015/2015"

# Turn off data.table and fst multithreading to prevent forking issues
# data.table::setDTthreads(1)
# fst::threads_fst(1)

#------------------ Load training data from /input --------------------------------------------

# Load the training data
train.data <- fst::read_fst(file.path(dir, list.files(dir, "train\\.fst$")))

# Load results of prepXY() with fusion and predictor variable details
prep <- readRDS(file.path(dir, list.files(dir, "prep\\.rds$")))

#------------------ Train fusion model and save to /output ------------------------------------------------------

# Plausible hyper-parameters
hyper.params <- list(
  boosting = "goss",
  num_leaves = 2 ^ (4:6),
  min_data_in_leaf = round(max(20, nrow(train.data) * 0.001)),
  num_iterations = 1000,
  feature_fraction = 0.8,
  learning_rate = 0.05
)

# Train fusion model
fsn.path <- train(data = train.data,
                  y = prep$y,
                  x = prep$x,
                  fsn = paste0(out.path, "model.fsn"),
                  weight = "weight",
                  cores = ncores,
                  hyper = hyper.params)

#------------------ Fuse - validation ------------------------------------------

# PERHAPS USEFUL for server runs using forking...
# Once train() is complete, reset number of threads allowed in data.table and fst
# data.table::setDTthreads(num.cores)
# fst::threads_fst(num.cores)

# Fuse multiple implicates to training data for internal validation analysis
valid <- fuse(data = train.data,
              fsn = fsn.path,
              M = nimps,
              fsd = paste0(out.path, "valid.fsd"),
              cores = ncores)

# Pass 'valid' implicates to validate() function
# Subset variables: income; race/ethnicity; education; household size; housing tenure; and a location variable
validation <- validate(observed = train.data,
                       implicates = read_fsd(valid),
                       subset_vars = attr(prep, "xforce"),
                       weight = "weight",
                       cores = ncores)

# Save 'validation' results as .rds
saveRDS(validation, file = paste0(out.path, "validation.rds"))

# Clean up
rm(train.data, prep, valid, validation)

#------------------ Fuse - simulation ------------------------------------------

# Load the prediction data
predict.data <- fst::read_fst(file.path(dir, list.files(dir, "predict\\.fst$")))

# Fuse multiple implicates to ACS and save results to disk as compressed .csv
fuse(data = predict.data,
     fsn = fsn.path,
     M = nimps,
     fsd = paste0(out.path, "fused.fsd"),
     cores = ncores)

# Clean up
rm(predict.data)
gc()

# If you want to check the fusion output...
#test <- read_fsd(paste0(out.path, "fused.fsd"))
