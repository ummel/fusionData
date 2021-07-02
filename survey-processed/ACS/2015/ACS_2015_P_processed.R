library(fusionData)
library(tidyverse)
source("R/utils.R")

#-----

# Load generic codebook processing function
source("survey-processed/ACS/processACScodebook.R")

# Process 2015 codebook into standard format
codebook <- processACScodebook("survey-raw/ACS/2015/PUMSDataDict15.txt")

#-----

# Unzip raw .zip file
unzip("survey-raw/ACS/2015/csv_pus.zip", exdir = tempdir(), overwrite = TRUE)
pus.files <- list.files(path = tempdir(), pattern = "pus..csv$", full.names = TRUE)

# Read household PUMS data
d <- pus.files %>%
  map_dfr(data.table::fread) %>%
  as_tibble() %>%
  rename_with(toupper)  # Ensure upper-case names for consistency for 'codebook'; replicate weights are sometimes lower-case in the raw data

# Replace literal empty strings ("") with NA for character type columns
# fread() does not convert empty strings to NA, as they are ambiguous
for (i in 1:ncol(d)) {
  if (is.character(d[[i]])) d[[i]] <- na_if(d[[i]], "")
}

# Delete temporary files
unlink(pus.files)
gc()

#-----

# Apply 'ADJINC' adjustment to appropriate variables
v.adjinc <- filter(codebook, var %in% names(d) & adj == "ADJINC")$var
d <- d %>%
  mutate_at(v.adjinc, ~ .x * (ADJINC / 1e6))

# Remove any variables lacking variation (this drops ADJINC)
d <- d %>%
  select_if(~ length(unique(.x)) > 1)

# Standardize state and PUMA variable and remove Puerto Rico observations
d <- d %>%
  filter(ST %in% 1:56)  # Ensure observations restricted to U.S. states and D.C.
#select(-DIVISION, -REGION)  # Not present in 2015 microdata

gc()

#-----

# Fix-up codebook for person records

codebook <- codebook %>%
  filter(var %in% names(d)) %>%
  add_count(var) %>%
  filter(!(n > 1 & is.na(value) & var %in% names(which(!map_lgl(d, anyNA))))) %>%  # Remove entries where value = NA but there are no NA's in the actual data
  mutate(
    label = ifelse(var == "LANP" & is.na(value), "English", label),  # Manual edit: Based on questionnaire, it looks like NA's for LANP are English speakers
    label = ifelse(var == "CITWP" & is.na(value), "Not naturalized", label),
    label = ifelse(var == "DRAT" & is.na(value), "Never served in military", label),
    label = ifelse(var == "MIL" & is.na(value), "Never served in military", label),
    label = ifelse(var == "OC" & is.na(value), "No", label),
    label = ifelse(var == "RC" & is.na(value), "No", label),
    label = ifelse(var == "SCH" & is.na(value), "No, has not attended in the last 3 months", label),
    label = ifelse(var == "SCHL" & is.na(value), "No schooling completed", label),
    label = ifelse(var == "VPS" & is.na(value), "Never served in military", label),
    label = ifelse(var == "YOEP" & is.na(value), "Born in the U.S.", label),

    desc = ifelse(var == "CITWP", "Year of naturalization", desc),
    desc = ifelse(desc == "VA", "Veterans Administration health care", desc)

  )

# Check for possible remaining issues in 'codebook'
#filter(codebook, is.na(value), label == "")

#----------------

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgment involved

ordered.factors <- c(
  'CITWP',
  'DECADE',
  'DRAT',
  'DRIVESP',
  'ENG',
  'GCM',
  'JWAP',
  'JWDP',
  'JWRIP',
  'MARHYP',
  'SCHG',
  'SCHL',
  'WKL',
  'YOEP'
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

# No or very few NA's remaining...
# This should retain NA's only for "suppressed" observations
# There are so few that it doesn't make sense to do full imputation
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

# Simple random imputation of remaining NA's (if any)
for (v in names(na.count)) {
  ind <- is.na(d[[v]])
  d[[v]][ind] <- sample(na.omit(d[[v]]), size = sum(ind), prob = d$PWGTP[!ind], replace = TRUE)
}

#----------------

# Assemble final output
# NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
d <- d %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  mutate(
    ST = factor(str_pad(ST, width = 2, pad = 0)),   # Standard geographic variable definitions for 'state' and 'puma10' (renamed below)
    PUMA = factor(str_pad(PUMA, width = 5, pad = 0))
  ) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = TRUE) %>%
  rename(
    acs_2015_hid = SERIALNO,  # Rename ID and weight variables to standardized names
    pid = SPORDER,
    weight = PWGTP,
    state = ST,
    puma10 = PUMA
  ) %>%
  rename_with(~ gsub("PWGTP", "REP_", .x, fixed = TRUE), .cols = starts_with("PWGTP")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(acs_2015_hid, pid, weight, everything(), -starts_with("rep_"), starts_with("rep_"))  # Reorder columns with replicate weights at the end

# Manual removal of variables without useful information
d <- d %>%
  select(-anc, -dratx, -mig, -racnum, -sciengp, -sciengrlp)

# Remaining manual fix-ups
labelled::var_label(d$puma10) <- "Public use microdata area code based on 2010 census definition"

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = d, survey = "ACS", vintage = 2015, respondent = "P")
saveRDS(object = dictionary, file = "survey-processed/ACS/2015/ACS_2015_P_dictionary.rds")
gc()

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/ACS/2015/ACS_2015_P_processed.fst", compress = 100)
