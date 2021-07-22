# Called from within compileCEI()

processMEMI <- function(survey_years, codebook) {

  d <- paste0("survey-processed/CEX/CEI/", survey_years, "/memi_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year,
           cuid = as.integer(str_sub(NEWID, 1, -2)),
           intnum = as.integer(str_sub(NEWID, -1, -1))) %>%
    rename_with(tolower) %>%
    select(-newid)

  #----------------

  # Subset 'codebook' object in global environment to restrict to variables in 'd'

  codebook <- codebook %>%
    filter(var %in% names(d))

  #----------------

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
  # Valid blanks were set to "Nursery, kindergarten, or elementary"; this sets the youngest children (age < 5) to "No schooling completed"
  d <- d %>%
    mutate(educa = replace(educa, age < 5, levels(d$educa)[1]))

  #----------------

  # Impute missing
  # There are so few NA's in the MEMI data (currently), it makes sense to impute separately from the FMLI and expenditure variables
  imp <- imputeMissing(data = d, N = 1)

  # Add imputed variables to 'd'
  d <- d %>%
    select(-any_of(names(imp))) %>%
    cbind(imp)

  return(d)

}
