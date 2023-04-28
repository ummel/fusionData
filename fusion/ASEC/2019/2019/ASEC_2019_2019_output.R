# File for performing ASEC fusion
# designed to be run locally and on SAVIO

# global arguments
rm(list=ls())
args = commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 0){
 args = c(T, T, 1)
 # 1 = running locally?
 # 2 = running test mode?
 # 3 = number of cores for server
}
LOCAL = args[1]
TEST = args[2]
ncores = as.numeric(args[3])

# package - make sure is up to date with
#devtools::install_github("ummel/fusionModel")
library(fusionModel)

# arguments ----

# set file directory 
if (LOCAL == T){
  if (TEST == T) input.dir <- "fusion_/ASEC/2019/2019/H/input/"
  if (TEST == F) input.dir <- "fusion/ASEC/2019/2019/H/input/"
}else{
  if (TEST == T) input.dir <- "/global/scratch/users/heroashman/fusionACS/test/input/"
  if (TEST == F) input.dir <- "/global/scratch/users/heroashman/fusionACS/input/"
}

# arguments to fusionOutput()
# depends on running test or not 
if (TEST == T){
  M = 1 # number of implicates
  note = "ASEC test"
}else{
  M = 30
  note = "ASEC run"
}


# RUN ----
if (LOCAL == T){
  # always running in test mode if local 
  output.dir <- fusionOutput(
    input = input.dir, 
    output = NULL, 
    M = 2, 
    note = "ASEC - local test",
    test_mode = TRUE, 
    validation = FALSE,
    margin = 4,
    ncores = 1)
  
  # getting error: Error in lightgbm::lgb.cv(params = c(as.list(x), params.obj), data = dfull,  : 
  # ‘folds’ must be a list with 2 or more elements that are vectors of indices for each CV-fold
  
}

if (LOCAL == F)
  output.dir <- fusionOutput(
    input = input.dir, # Path to server directory containing file created by fusionInput()
    output = NULL,  # Path to server directory where you want the output files to go
    M = M,  # 30 is sufficient.
    note = note,
    test_mode = TEST,  # I would run with test_mode=TRUE once first to test
    validation = FALSE,  # I think we can skip validation.
    ncores = ncores,  # This depends on the available memory of the server. If set too high, memory will be exhausted.
    margin = 4,
    fork = T  # Will want to put fork=TRUE here to pass to train(). Otherwise, it will run in multithread mode rather than forked parallel (latter is faster). See documentation.
)



