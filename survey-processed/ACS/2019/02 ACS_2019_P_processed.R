library(tidyverse)
source("R/utils.R")

#-----

# Load generic codebook; creates "codebook" object
source("survey-processed/ACS/2019/01 Pre-process data dictionary.R")

#-----

# Unzip raw .zip file
unzip("survey-raw/ACS/2019/csv_pus.zip", exdir = tempdir(), overwrite = TRUE)
pus.files <- list.files(path = tempdir(), pattern = "_pus..csv$", full.names = TRUE)

# Read household PUMS data
d <- pus.files %>%
  map_dfr(data.table::fread) %>%
  as_tibble()

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

# Standardize PUMA variable and remove Puerto Rico observations
d <- d %>%
  filter(ST %in% 1:56) %>%  # Ensure observations restricted to U.S. states and D.C.
  mutate(PUMA = paste0(str_pad(ST, width = 2, pad = 0), str_pad(PUMA, width = 5, pad = 0))) %>%
  select(-ST, -DIVISION, -REGION)

# Standardize PUMA codes for Migration and Place of work variables (MIGPUMA & POWPUMA) to include 2-digit state FIPS at front
# Can then drop the associated state identifiers (MIGSP & POWSP)
d <- d %>%
  mutate(MIGPUMA = paste0(str_pad(MIGSP, 2, pad = 0), str_pad(MIGPUMA, width = 5, pad = 0)),
         MIGSP = NULL,
         POWPUMA = paste0(str_pad(POWSP, 2, pad = 0), str_pad(POWPUMA, width = 5, pad = 0)),
         POWSP = NULL)

# Only retain variables found in the codebook
# This principally drops flag variables
d <- d[intersect(names(d), codebook$var)]

gc()

#-----

# Fix-up codebook for person records

codebook <- codebook %>%
  filter(var %in% names(d)) %>%
  add_count(var) %>%
  filter(!(n > 1 & is.na(value) & var %in% names(which(!map_lgl(d, anyNA))))) %>%
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

    # label = ifelse(var == "WRK" & is.na(value), "Not reported", label),  # Retain legit "Not reported" entry for "Worked last week?" variable
    # label = ifelse(var == "ANC" & value == 4, "Not reported", label),  # Retain legit "Not reported" entries for ancestry variables
    # label = ifelse(var == "ANC1P" & value == 999, "Not reported", label),  # Retain legit "Not reported" entries for ancestry variables
    # label = ifelse(var == "ANC2P" & value == 999, "Not reported", label)  # Retain legit "Not reported" entries for ancestry variables

    # Convert departure/arrival time at work variables to numeric minutes past midnight - NOT TERRIBLY USEFUL; still has to be treated as ordered factor
    # tdiff = as.POSIXct(gsub(".", "", map_chr(str_split(label, " to "), 1), fixed = TRUE), format = "%I:%M %p") - as.POSIXct("12:00 am", format = "%I:%M %p"),
    # label = ifelse(var %in% c("JWAP", "JWDP") & !is.na(value), as.double(tdiff, units = "mins") , label),  # Minutes past midnight
    # tdiff = NULL,
    # desc = ifelse(var == "JWAP", "Time of arrival at work (minutes past midnight)", desc),
    # desc = ifelse(var == "JWDP", "Time of departure for work (minutes past midnight)", desc)
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

  # Ensure unordered factor levels are sorted alphabetically
  if (is.factor(x) & !is.ordered(x)) x <- factor(x, levels = sort(unique(x)))

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
  labelled::set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = TRUE) %>%
  rename(
    acs_2019_hid = SERIALNO,  # Rename ID and weight variables to standardized names
    pid = SPORDER,
    weight = PWGTP
  ) %>%
  rename_with(~ gsub("PWGTP", "REP_", .x, fixed = TRUE), .cols = starts_with("PWGTP")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(acs_2019_hid, pid, weight, everything(), -starts_with("rep_"), starts_with("rep_"))  # Reorder columns with replicate weights at the end

# Manual removal of variables without useful information
d <- d %>%
  select(-anc, -dratx, -mig, -racnum, -sciengp, -sciengrlp)

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = d, survey = "ACS", vintage = 2019, respondent = "P")
saveRDS(object = dictionary, file = "survey-processed/ACS/2019/ACS_2019_P_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/ACS/2019/ACS_2019_P_processed.fst", compress = 100)
