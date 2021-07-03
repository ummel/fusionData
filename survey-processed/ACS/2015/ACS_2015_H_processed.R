library(tidyverse)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

# Load generic codebook processing function
source("survey-processed/ACS/processACScodebook.R")

# Load utility cost flag function (only prior to 2018)
source("survey-processed/ACS/utilityCostFlags.R")

#-----

# Process 2015 codebook into standard format
codebook <- processACScodebook("survey-raw/ACS/2015/PUMSDataDict15.txt")

#-----

# Unzip raw .zip file
unzip("survey-raw/ACS/2015/csv_hus.zip", exdir = tempdir(), overwrite = TRUE)
hus.files <- list.files(path = tempdir(), pattern = "hus..csv$", full.names = TRUE)

# Read household PUMS data
d <- hus.files %>%
  map_dfr(data.table::fread) %>%
  as_tibble() %>%
  rename_with(toupper)  # Ensure upper-case names for consistency for 'codebook'; replicate weights are sometimes lower-case in the raw data

# Replace literal empty strings ("") with NA for character type columns
# fread() does not convert empty strings to NA, as they are ambiguous
for (i in 1:ncol(d)) {
  if (is.character(d[[i]])) d[[i]] <- na_if(d[[i]], "")
}

# Delete temporary files
unlink(hus.files, recursive = TRUE)
gc()

#-----

# The only variable in 'hus' that contains useful information about group quarter individuals is "FS" (Did anyone in household receive SNAP?)
# Idea: Could create 'FS' version in the 'pus' data, with 1's if the household received SNAP and 0 otherwise...

# Apply 'ADJHSG' and 'ADJINC' adjustment to appropriate variables
v.adjhsg <- filter(codebook, var %in% names(d) & adj == "ADJHSG")$var
v.adjinc <- filter(codebook, var %in% names(d) & adj == "ADJINC")$var
d <- d %>%
  mutate_at(v.adjhsg, ~ .x * (ADJHSG / 1e6)) %>%
  mutate_at(v.adjinc, ~ .x * (ADJINC / 1e6))

# For now, remove GQ observations from 'hus'
# And remove any variables lacking variation (this drops ADJHSG and ADJINC)
d <- d %>%
  filter(TYPE == 1, NP > 0) %>%  # Remove vacant units, too
  select_if(~ length(unique(.x)) > 1)

# Standardize PUMA variable and remove Puerto Rico observations
d <- d %>%
  filter(ST %in% 1:56) %>%  # Ensure observations restricted to U.S. states and D.C.
  select(-DIVISION, -REGION)

gc()

#-----

# Fix-up codebook for household records
codebook <- codebook %>%
  filter(var %in% names(d)) %>%
  add_count(var) %>%
  filter(!(n > 1 & is.na(value) & var %in% names(which(!map_lgl(d, anyNA))))) %>%
  filter(!var %in% c("SRNT", "SVAL")) %>%   # Manual removal of variables without useful information
  mutate(
    label = ifelse(var == "RNTM" & is.na(value), "No", label),
    desc = ifelse(var == "FS", "Food stamp recipient in household", desc),
    desc = ifelse(var == "HHT2", "Household/family type, including cohabiting", desc),
    desc = str_to_sentence(desc)  # This works OK for 'hus' but not for 'pus' variable descriptions
  )

# Check for possible remaining issues in 'codebook'
# NOTE: I cannot find a good explanation for the remaining NA's for the variables identified below
# The codebook claims NA's should only be found for GQ's, but NA's remain after GQ observations are removed
# I am treating these NA's as legitimate missing and they are imputed further down in code
filter(codebook, is.na(value), label == "")

# Set blank labels to legitimate NA (see note above)
codebook <- codebook %>%
  mutate(label = na_if(label, ""))

#----------------

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgment involved

ordered.factors <- c(
  'ACR',
  'AGS',
  'BLD',
  'MV',
  'R18',
  'R60',
  'R65',
  'VEH',
  'WIF',
  'YBL'
)

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(ordered.factors, codebook$var))
stopifnot(length(extras) == 0)

#----------------

# Only retain variables remaining in the codebook
d <- d[intersect(names(d), codebook$var)]

#----------------

# Update variable values with associated labels from 'codebook'

# Loop through each variable in 'd', assigning labels when applicable
for (v in names(d)) {

  cb <- filter(codebook, var == v)
  x <- d[[v]]
  y <- unlist(cb$value)
  z <- unlist(cb$label)
  m <- match(x, y)

  # Update 'x' with new value labels
  new.labels <- z[na.omit(m)]
  x[!is.na(m)] <- new.labels

  # Coerce result to ordered factor, if specified
  # Note that levels are restricted to those actually present in the data
  if (v %in% ordered.factors) {
    num.na <- sum(is.na(x))
    x <- factor(x, levels = intersect(z, x), ordered = TRUE)
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Apply type.convert() to 'x'; leave ordered factors unchanged
  x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

  # Ensure unordered factor levels are sorted according to codebook order of levels
  # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
  # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
  if (is.factor(x) & !is.ordered(x)) {
    num.na <- sum(is.na(x))
    if (all(x %in% cb$label)) {
      x <- factor(x, levels = intersect(cb$label, unique(x)))
    } else {
      x <- factor(x, levels = sort(unique(x)))
    }
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Update column in 'd'
  d[[v]] <- x

}

gc()

#----------------

# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

# Use imputeMissing(), since the number are variety of NA's is non-trivial
# To avoid memory issues, 'x_exclude' is used to restrict predictors to a reasonable set of variables
# N = 1 means there is only one imputation iteration; more than one can cause memory issues
imp <- imputeMissing(data = d,
                     N = 1,
                     weight = "WGTP",
                     x_exclude = setdiff(names(d), c("NP", "BLD", "ELEP", "HFL", "OCPIP", "RMSP", "GRPIP", "TEN", "VALP", "VEH", "YBL", "HINCP", "WIF", "R18", "R65")))

# Replace NA's in 'd' with the imputed values
d[names(imp)] <- imp
rm(imp)
gc()

# Simple random imputation of missing values
# This is appropriate if the number of NA's is low and the variables requiring imputation are not particularly related
# for (v in names(na.count)) {
#   ind <- is.na(d[[v]])
#   d[[v]][ind] <- sample(na.omit(d[[v]]), size = sum(ind), prob = d$WGTP[!ind], replace = TRUE)
# }

anyNA(d)

#----------------

# Assemble final output
# NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
d <- d %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  mutate(
    ST = factor(str_pad(ST, width = 2, pad = 0)),   # Standard geographic variable definitions for 'state' and 'puma10' (renamed below)
    PUMA = factor(str_pad(PUMA, width = 5, pad = 0))
  ) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var)) %>%
  rename(
    acs_2015_hid = SERIALNO,  # Rename ID and weight variables to standardized names
    weight = WGTP,
    state = ST,
    puma10 = PUMA
  ) %>%
  rename_with(~ gsub("WGTP", "REP_", .x, fixed = TRUE), .cols = starts_with("WGTP")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(acs_2015_hid, weight, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(acs_2015_hid)

# Manual removal of variables without useful information
# d <- d %>%
#   select(-srnt, -sval)

# Add utility cost flag variables (only prior to 2018)
# See "utilityCostFlags.R" for details
d <- utilityCostFlags(d)

# Remaining manual fix-ups
labelled::var_label(d$dsl) <- "DSL service"
labelled::var_label(d$ocpip) <- "Selected monthly owner costs as a percentage of household income during the past 12 months"
labelled::var_label(d$puma10) <- "Public use microdata area code based on 2010 census definition"
labelled::var_label(d$tel) <- "Telephone service"

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = d, survey = "ACS", vintage = 2015, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/ACS/2015/ACS_2015_H_dictionary.rds")
gc()

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/ACS/2015/ACS_2015_H_processed.fst", compress = 100)
