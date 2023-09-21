library(fusionData)
library(fusionModel)

# Number of cores to use
ncores <- 3

# Donor and recipient survey identifiers
donor <- "AHS_2019"
recipient <- "ACS_2016"

# Directory in /fusion where results will be saved
# This is automatically constructed from 'donor' and 'recipient', assuming recipient is ACS-based
acs.vintage <- substring(recipient, 5)
dir <- paste("fusion", sub("_", "/", donor), acs.vintage, "input", sep = "/")

#-----

# Prepare and assemble data inputs
prep <- prepare(donor = donor,
                recipient = recipient,
                respondent = "household",
                implicates = 5)

# Specify fusion variables to be retained in harmonization results
# Removed pca for prep 
data <- assemble(prep,
                 fusion.variables = c("cold","hmreneff",
                                      'fsafford','fseatless','fshungry','fslostwgt','fsstatus'), #unitsize included for validation with CoreLogic 
                 spatial.datasets = "all",
                 window = 2)

rm(prep)

#-----

# Create custom fusion variables
# These are useful comb%>%

# Identify the 'fusion.vars'

# Temporary test of blocked variables
# Manually assign the "*_share" fusion variables to a block
# This is because they must sum to 1 at the household level
#fusion.vars <- c(as.list(fusion.vars[1:8]), list(fusion.vars[9:11]))
fusion.vars <- setdiff(setdiff(names(data[[1]]), names(data[[2]])), "ahs_2019_hid")

#-----

# Specify the predictors in the harmonized donor data that will be used for validation subsets
# We select the variables that best reflect the following socioeconomic and geographic concepts:
#  -- income; race/ethnicity; education; household size; housing tenure; and a relatively high-resolution location variable
# These variables are "forced" as predictors in prepXY() and carried along in 'prep' for use by validate() in /output.R
sub.vars <- c("hincp__hincp", "hhrace__rac1p", "grad__schl",
              "numpeople__np", "tenure__ten", "loc..division")

# Identify the shared 'pred.vars'
pred.vars <- setdiff(intersect(names(data[[1]]), names(data[[2]])), "weight")

# Determine fusion order and subset of 'pred.vars' to use with each fusion variable/block
prep <- prepXY(data = data[[1]],
               y = fusion.vars,
               x = pred.vars,
               weight = "weight",
               xforce = sub.vars,
               cores = ncores)

# Save output from prepXY()
saveRDS(prep, file = file.path(dir, paste(donor, acs.vintage, "prep_UP.rds", sep = "_")))

#-----

# Update 'pred.vars' to the subset of predictors retained in 'prep'
pred.vars <- attr(prep, "xpredictors")

# Set cores for 'fst' to use when writing to disk
fst::threads_fst(ncores)

# Save training data to disk
data[[1]] %>%
  dplyr::select(one_of(c("weight", unlist(prep$y), pred.vars))) %>%
  fst::write_fst(path = file.path(dir, paste(donor, acs.vintage, "train_UP.fst", sep = "_")), compress = 100)

# Save prediction data to disk
data[[2]] %>%
  dplyr::select(one_of(pred.vars)) %>%
  fst::write_fst(path = file.path(dir, paste(donor, acs.vintage, "predict_UP.fst", sep = "_")), compress = 100)
