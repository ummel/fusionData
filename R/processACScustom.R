#' @rdname processACSmicrodata
#' @aliases NULL
#'
#' @details
#' \bold{processACScustom Mechanics & Script Requirements:}
#' Standard PUMS microdata often lacks domain-specific or recoded metrics required for downstream
#' modeling (e.g., custom poverty ratios, recoded race/ethnicity categories, housing cost burdens).
#' \code{processACScustom()} automates the execution and validation of these derived variables:
#' \enumerate{
#'   \item \strong{Script Scanning}: Scans \code{survey-processed/ACS/custom/*.R} for individual R scripts.
#'   \item \strong{Function Contract}: Each custom script must define a function named identically to
#'         its filename (e.g., \code{poverty.R} defines \code{poverty()}). The function must accept
#'         \code{year} as its sole argument and return a data frame containing primary keys (\code{hid},
#'         and optionally \code{pid}).
#'   \item \strong{Validation Checks}: Guarantees that zero missing (\code{NA}) values exist in output
#'         columns, verifies that explicit variable labels are assigned via \code{\link[labelled]{var_label}},
#'         filters records to occupied housing units, and prevents column name collisions across scripts.
#' }
#'
#' @export

# Process and Compile Custom ACS Microdata Variables
#
# PURPOSE & OVERVIEW FOR USERS:
# The processACScustom() function is an internal, non-exported maintainer utility
# in the fusionData pipeline. Standard ACS microdata processed via processACSmicrodata()
# contains variables natively provided by the U.S. Census Bureau. However, fusionACS
# workflows frequently require user-defined, derived, or harmonized "custom" variables
# (e.g., specific housing classifications, income adjustments) that are not present in raw Census files.
#
# HOW IT WORKS:
# This function dynamically scans for individual R script files stored within
# 'survey-processed/ACS/custom/'. Each custom R script must define a function named
# identically to its filename, accepting 'year' as its single argument, and returning
# a data frame containing custom variables along with primary keys ('year', 'hid', and
# optionally 'pid').
#
# KEY PROCESSING PHASES:
#   1. Script Ingestion & Validation: Iterates through each .R file in the custom directory,
#      sources it, verifies argument constraints, and executes the script for the specified vintage.
#   2. Level Detection & Universe Harmonization: Checks output keys ('hid' vs. 'pid') against
#      previously processed ACS microdata files to automatically distinguish person-level
#      from household-level data and filters household observations to occupied units.
#   3. Label Enforcement & Cleaning: Validates that all derived custom variables possess explicit
#      variable labels via labelled::var_label(), formats numeric tolerances, and cleans factors.
#   4. Aggregation & Conflict Resolution: Combines output across multiple custom scripts
#      separately for person- and household-level records, checking for naming collisions.
#   5. Dictionary Updating & Storage: Appends custom variable definitions into the primary
#      data dictionary (.rds) with custom=TRUE tags and saves custom microdata sidecars (.fst).
#
# EXPECTED INPUTS & DIRECTORY STRUCTURE:
#   - 'year': Integer survey vintage (e.g., 2024).
#   - Custom R scripts located in 'survey-processed/ACS/custom/*.R'. Each function must:
#       - Accept 'year' as its sole argument.
#       - Return a data frame containing 'hid' (household ID) and optionally 'pid' (person ID).
#       - Ensure zero missing (NA) values in the output.
#       - Include variable labels assigned via labelled::var_label().
#   - Previously generated processed microdata files in 'survey-processed/ACS/{year}/'
#     (e.g., ACS_{year}_P_processed.fst and ACS_{year}_H_processed.fst).
#
# OUTPUTS GENERATED:
# Updates and creates sidecar files in 'survey-processed/ACS/{year}/':
#   - Updates 'ACS_{year}_{H|P}_dictionary.rds': Appends custom variable metadata.
#   - Creates 'ACS_{year}_{H|P}_custom.fst': Standalone microdata store holding custom variables.

processACScustom <- function(year) {

  if (basename(getwd()) != "fusionData") {
    cli::cli_abort("Function must be executed from within the {.file fusionData} directory.")
  }

  cli::cli_h1("Processing custom ACS variables for vintage {year}")

  # Identify all custom calculation R scripts available in the custom directory
  flist <- list.files("survey-processed/ACS/custom", pattern = "\\.R$", full.names = TRUE)

  if (length(flist) == 0) {
    cli::cli_alert_warning("No custom function scripts found in 'survey-processed/ACS/custom'")
    return(invisible(NULL))
  }

  # Execute and evaluate each custom processing script individually
  out <- lapply(flist, function(fun) {

    # Extract function name from path, load code into environment, and validate formals
    source(fun)
    fname <- sub("\\.R$", "", basename(fun))
    f <- get(fname)
    a <- names(formals(f))
    if (length(a) != 1 | a[1] != "year") {
      stop("The custom function ", fname, "() must have 'year' as its lone argument", sep = "")
    }

    # Execute custom computation routine wrapped in error handling
    cli::cli_alert_info("Executing custom function from {.file {basename(fun)}} for year {year}")
    out <- try(expr = f(year = year), silent = TRUE)

    # Process and validate successfully evaluated data frame returns
    if (inherits(out, "data.frame")) {

      # Enforce mandatory metadata and key structures
      out$year <- as.integer(year)
      if (!'hid' %in% names(out)) stop("Custom function in ", basename(fun), " must return 'hid' variable in result")
      if (anyNA(out)) stop("NA values are not allowed in the custom function output from ", basename(fun))

      # Standardize row ordering across primary keys to support deterministic comparisons
      out <- arrange(out, across(any_of(c('hid', 'pid'))))

      # Compare record identifiers against baseline processed microdata to determine data level
      pfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_P_processed.fst"), recursive = TRUE, full.names = TRUE)
      if (length(pfile) == 0) stop("Could not locate processed person microdata file for year ", year)

      p.hid <- fst::read_fst(pfile, columns = "hid")[[1]]

      if ('pid' %in% names(out) & identical(p.hid, out$hid)) {
        cli::cli_alert_info("  -- Classifying output as person-level records")
      } else {
        h.hid <- fst::read_fst(sub("_P_", "_H_", pfile), columns = "hid")[[1]]
        if (!'pid' %in% names(out) & all(h.hid %in% out$hid)) {
          cli::cli_alert_info("  -- Classifying output as household-level records")
          # Filter household records to match active occupied unit universe
          out <- filter(out, hid %in% h.hid)
        } else {
          stop("Unable to align output from ", basename(fun), " with standard person or household record keys")
        }
      }

      # Confirm that all newly introduced custom variables possess required descriptive labels
      cvars <- setdiff(names(out), c('year', 'hid', 'pid'))
      labs <- labelled::var_label(out)[cvars]
      miss <- setdiff(cvars, names(labs))
      if (length(miss)) stop("The following custom variables in ", basename(fun), " are missing labels: ", paste(miss, collapse = ", "))

      # Clean types, format floating points within tolerance, and re-apply variable metadata
      out <- out %>%
        mutate_if(is.factor, safeCharacters) %>%
        mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
        labelled::set_variable_labels(.labels = labs, .strict = FALSE) %>%
        arrange(across(any_of(c('hid', 'pid'))))

      cli::cli_alert_success("  -- Custom script {.file {basename(fun)}} successfully executed")
      return(out)

    } else {
      # Log execution failures gracefully and return NULL to be filtered out
      cli::cli_alert_danger("  -- Custom function {.file {basename(fun)}} failed execution")
      return(NULL)
    }

  })

  #---

  cli::cli_alert_info("Merging outputs across custom calculation scripts")

  # Remove failed execution results
  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) {
    cli::cli_alert_warning("No custom functions executed successfully. Exiting without saving.")
    return(invisible(NULL))
  }

  # Segregate and recursively combine custom calculations by respondent level
  pi <- map_lgl(out, ~ "pid" %in% names(.x))

  # Merge person-level custom variables on year, hid, and pid
  p <- if (any(pi)) reduce(out[pi], left_join, by = c('year', 'hid', 'pid'), relationship = 'one-to-one', suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(p), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during person-level merge: ", paste(dupe, collapse = ", "))

  # Merge household-level custom variables on year and hid
  h <- if (any(!pi)) reduce(out[!pi], left_join, by = c('year', 'hid'), relationship = 'one-to-one', suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(h), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during household-level merge: ", paste(dupe, collapse = ", "))

  # Write combined results to disk and update global data dictionaries
  for (i in c('h', 'p')) {
    d <- get(i)
    if (!is.null(d)) {

      level_str <- ifelse(i == "h", "Household", "Person")

      # Build dictionary data structure reflecting custom additions
      cli::cli_alert_info("Generating dictionary for custom {level_str}-level variables")
      dict <- fusionData::createDictionary(data = d, survey = "ACS", vintage = year, respondent = toupper(i), custom = TRUE)

      # Read base dictionary, strip previous custom entries if re-running, append new entries, and save
      cli::cli_alert_info("Updating dictionary file on disk")
      fname <- file.path("survey-processed/ACS", year, paste("ACS", year, toupper(i), "dictionary.rds", sep = "_"))

      # Retrieve non-custom variables directly from primary processed microdata file
      vars <- names(fst::fst(sub("dictionary.rds", "processed.fst", fname)))

      readRDS(fname) %>%
        filter(variable %in% vars) %>%
        mutate(custom = FALSE) %>%
        bind_rows(dict) %>%
        arrange(variable) %>%
        saveRDS(file = fname)

      # Export custom variables microdata archive to sidecar .fst file
      cli::cli_alert_info("Saving custom {level_str}-level microdata to disk")
      fst::write_fst(x = d, path = sub("dictionary.rds", "custom.fst", fname), compress = 100)

    }
  }

  cli::cli_alert_success("Custom ACS variable processing complete for vintage {year}")

}
