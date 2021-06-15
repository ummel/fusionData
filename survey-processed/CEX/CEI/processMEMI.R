library(labelled)
library(tidyverse)
source("R/utils.R")

# # Load H2O model for EER pre-fitted to NHANES microdata (see: Create nhanes_eer_model.R)
# nhanes.fit <- h2o.loadModel("data/nhanes_eer_model")

#-----

processMEMI <- function(survey_years) {

  #d <- readRDS(paste0("data-raw/CEX/", survey_year, "/memi_", survey_year, ".rds")) %>%
  d <- paste0("survey-processed/CEX/", survey_years, "/memi_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year,
           cuid = as.integer(str_sub(NEWID, 1, -2)),
           intnum = as.integer(str_sub(NEWID, -1, -1))) %>%
    rename_with(tolower)

  #---

  # DROP for convenience; these are variables I thought might be interesting down the road but not critical to current project
  # drop <- c("emplcont", "govretx", "indretx", "jssdedxm", "privpenx", "slfempsm", "ssnorm")
  # ddrop <- intersect(names(d), drop)
  # d <- select(d, -ddrop)

  #----------------

  # Load codebook
  codebook <- paste0("survey-processed/CEX/", survey_years, "/dictionary_", survey_years, ".rds") %>%
    map_dfr(readRDS) %>%
    mutate(var = tolower(var)) %>%
    filter(var %in% names(d)) %>%
    group_by(var, value) %>%
    slice(1) %>%
    ungroup()

  # Clean up the codebook
  codebook <- codebook %>%
    mutate(

      # Manual edits to variable descriptions ('desc')
      desc = ifelse(var == "cucode", "Relation to reference person", desc),
      desc = ifelse(var == "educa", "Highest level of schooling member has completed or highest degree received", desc),
      desc = ifelse(var == "incnonwk", "Main reason member did not work during the past 12 months", desc),
      desc = ifelse(var == "incomey", "Employee type; refers to job with most earnings in the past 12 months", desc),
      desc = ifelse(var == "membno", "Member number within consumer unit", desc),
      desc = ifelse(var == "occucode", "Occupation type; refers to job with most earnings in the past 12 months", desc),
      desc = map_chr(strsplit(desc, ", mean of", fixed = TRUE), 1),
      desc = str_to_sentence(desc),

      # Manual edits to variable labels ('label')
      label = ifelse(var == "earner" & value == "1", "Member earns income", label),
      label = ifelse(var == "earner" & value == "2", "Member does not earn income", label),
      label = ifelse(var == "educa" & value == "1", "No schooling completed", label),
      label = ifelse(var == "educa" & value == "2", "Nursery, kindergarten, or elementary", label),
      label = ifelse(var == "educa" & value == "3", "High school, no degree", label),
      label = ifelse(var == "educa" & value == "4", "High school graduate (diploma or equivalent)", label),
      label = ifelse(var == "educa" & value == "5", "Some college, no degree", label),
      label = ifelse(var == "educa" & value == "6", "Associate's degree", label),
      label = ifelse(var == "educa" & value == "7", "Bachelor's degree", label),
      label = ifelse(var == "educa" & value == "8", "Master's, professional, or doctoral degree", label),
      label = str_to_sentence(label)
    ) %>%

    # Add manual entries to the codebook
    add_row(var = "cuid", desc = "Consumer unit unique identifier", value = NA, label = NA) %>%
    add_row(var = "intnum", desc = "Interview number", value = NA, label = NA) %>%
    mutate_all(str_squish)

  #----------------

  # Manually constructed...
  # What value should "Valid blank" take for the following variables?

  # Variables with "Valid blank" values
  # These are the variables for which suitable replacement values must be specified below
  na.vars <- d %>%
    map_lgl(~ any(.x == "Valid blank", na.rm = TRUE)) %>%
    which() %>%
    names()

  # Code to help discern valid blank values
  # table(d$hlfbathq)
  # View(filter(d, educa == "Valid blank"))
  # filter(codebook, var == "hlfbathq")

  na.values <- list(

    arm_forc = "No",
    earner = "Member does not earn income",
    educa = "Nursery, kindergarten, or elementary",  # See fix-up below young children
    in_coll = "Not at all",
    inc_hrsq = 0,
    incnonwk = "Member worked",
    incomey = "Member did not work",
    incweekq = 0,
    occucode = "Member did not work",
    salaryxm = 0,
    sempfrmm = 0,
    socrrxm = 0,
    ssixm = 0

  )

  # Safety check for missing entries in 'na.values'
  miss <- noquote(setdiff(na.vars, names(na.values)))
  stopifnot(length(miss) == 0)

  #----------------

  # Assign levels for ordered factors
  # In general, we want to coerce unordered factor to ordered factors whenever feasible
  # There is some judgment involved

  ordered.factors <- list(
    educa = c("No schooling completed", "Nursery, kindergarten, or elementary", "High school, no degree", "High school graduate (diploma or equivalent)", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Master's, professional, or doctoral degree"),
    in_coll = c("Not at all", "Part time", "Full time")
  )

  # Safety check
  # Detect any variables in 'ordered.factors' that are NOT in the codebook
  extras <- noquote(setdiff(names(ordered.factors), codebook$var))
  stopifnot(length(extras) == 0)

  # Safety check
  # Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
  # Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
  for (v in names(ordered.factors)) {
    of <- sort(ordered.factors[[v]])
    cb <- sort(unique(filter(codebook, var == v)$label))
    if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
  }

  #----------------

  # Update variable values with associated labels from 'codebook'

  # Loop through each variable in 'd', assigning labels when applicable
  for (v in names(d)) {

    cb <- filter(codebook, var == v)
    x <- d[[v]]
    y <- unlist(cb$value)
    z <- unlist(cb$label)
    if (v %in% names(na.values)) {
      y <- c(y, "Valid blank")
      z <- c(z, na.values[[v]])
    }
    m <- match(x, y)
    new.labels <- z[na.omit(m)]

    # Update 'x' with new value labels
    x[!is.na(m)] <- new.labels

    # Coerce result to ordered factor, if specified
    # Note that levels are restricted to those actually present in the data
    if (v %in% names(ordered.factors)) {
      num.na <- sum(is.na(x))
      x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
      stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }

    # Apply type.convert() to 'x'; leave ordered factors unchanged
    x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

    # Ensure unordered factor levels are sorted alphabetically
    if (is.factor(x) & !is.ordered(x)) x <- factor(x, levels = sort(unique(x)))

    # Update column in 'd'
    d[[v]] <- x

  }

  # Safety check
  # Ensure there are not "Valid blank" entries remaining in the data
  stopifnot(!any(d == "Valid blank", na.rm = TRUE))

  #----------------

  # The 'age' NA's have code "D" ("Valid value; unadjusted") but based on frequency of age values (and "cu_code" value), these appear to be actual zeros
  # See table(d$age) to look at frequencies; appears there are fewer age = 0 than we would expect
  # This is likely a BLS data error; overwriting for now
  d <- d %>%
    mutate(age = ifelse(is.na(age), 0, age))

  # Set 'educa' to plausible value based on member age
  # Valid blanks were set to "Nursery, kindergarten, or elementary"; this sets the youngest children to "No schooling completed"
  d <- d %>%
    mutate(educa = replace(educa, age < 5, levels(d$educa)[1]))

  #----------------

  # Impute missing
  # There are so few NA's in the MEMI data (currently), it makes sense to impute separately
  d <- imputeMissing(data = d, N = 1)

  #----------------

  # Assemble final output
  # Restrict final output to each CU's last/final interview
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
  final <- d %>%
    group_by(cuid) %>%
    filter(intnum == max(intnum)) %>%
    ungroup() %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = FALSE) %>%
    rename(
      cei_hid = cuid,  # Rename ID variables to standardized names
      pid = membno
    ) %>%
    select(-intnum, -survey_year, -newid) %>%
    select(cei_hid, pid, everything())

  return(final)

}
