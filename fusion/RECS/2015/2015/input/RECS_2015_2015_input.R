library(fusionData)
library(fusionModel)

# Number of cores to use
ncores <- 1

# Donor and recipient survey identifiers
donor <- "RECS_2015"
recipient <- "ACS_2015"

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
                 fusion.variables = c("btung", "btuel", "cooltype","scalee","scaleg",'scaleb','noheatng', "btufo", "btulp",
                                      "noacbroke","noacel","noheatel",'noheatbroke','noheatbulk'),
                 window = 2)


rm(prep)

#-----

# Create custom fusion variables
# These are useful combinations of original donor variables
data[[1]] <- data[[1]] %>%
  mutate(
    insec = scalee != "Never" | scaleg != "Never"| scaleb != "Never",
    noheat = noheatbroke == "Yes" | noheatbulk == "Yes" |  noheatel == "Yes" | noheatng == "Yes",
    noac = noacel == "Yes" | noacbroke == "Yes",
  ) %>%
  select(-all_of(c("scalee","scaleb",'scaleg', "noacel", "noacbroke", "noheatbroke",
                   "noheatbulk", "noheatel", "noheatng")))

# Identify the 'fusion.vars'
fusion.vars <- setdiff(setdiff(names(data[[1]]), names(data[[2]])), "recs_2015_hid")

# Temporary test of blocked variables
# Manually assign the "*_share" fusion variables to a block
# This is because they must sum to 1 at the household level
#fusion.vars <- c(as.list(fusion.vars[1:8]), list(fusion.vars[9:11]))

#-----

# Specify the predictors in the harmonized donor data that will be used for validation subsets
# We select the variables that best reflect the following socioeconomic and geographic concepts:
#  -- income; race/ethnicity; education; household size; housing tenure; and a relatively high-resolution location variable
# These variables are "forced" as predictors in prepXY() and carried along in 'prep' for use by validate() in /output.R
sub.vars <- c("moneypy__hincp", "householder_race__rac1p", "education__schl",
              "nhsldmem__np", "kownrent__ten", "loc..recs_division")

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
saveRDS(prep, file = file.path(dir, paste(donor, acs.vintage, "prep.rds", sep = "_")))

#-----

# Update 'pred.vars' to the subset of predictors retained in 'prep'
pred.vars <- attr(prep, "xpredictors")

# Set cores for 'fst' to use when writing to disk
threads_fst(ncores)

# Save training data to disk
data[[1]] %>%
  select(one_of(c("weight", unlist(prep$y), pred.vars))) %>%
  write_fst(path = file.path(dir, paste(donor, acs.vintage, "train.fst", sep = "_")), compress = 100)

# Save prediction data to disk
data[[2]] %>%
  select(one_of(pred.vars)) %>%
  write_fst(path = file.path(dir, paste(donor, acs.vintage, "predict.fst", sep = "_")), compress = 100)
