#' Generate Fusion Input Datasets from Harmonized Donor and ACS Microdata
#'
#' @description
#' Constructs and formats the aligned donor (training) and ACS recipient (prediction)
#' microdata datasets required for downstream statistical fusion in the `fusionData`
#' workflow. `fusionInput()` orchestrates survey harmonization, geographic location
#' (PUMA) imputation, predictor distribution quality screening, and numeric feature
#' scaling before writing compressed binary `.fst` files to disk.
#'
#' @param donor Character. Identifier for the donor survey and vintage (e.g. `"RECS_2015"`,
#'   `"AHS_2023"`).
#' @param acs_year Integer. Year of ACS microdata serving as the recipient dataset (e.g. `2023`).
#' @param respondent Character. Unit of observation; must be either `"household"` (or `"H"`)
#'   or `"person"` (or `"P"`).
#' @param test_mode Logical. If `TRUE` (default), outputs are written to a scratch directory
#'   (`fusionData/fusion_/`) and datasets are truncated to ~10,000 observations for rapid testing.
#'   If `FALSE`, full-scale production files are written to `fusionData/fusion/` (no underscore).
#' @param ncores Integer. Number of CPU cores allocated for parallel execution during
#'   harmonization and file compression. Defaults to `getOption("fusionData.cores")`.
#' @param note Character. Optional user note recorded directly in the run execution log.
#'   Defaults to `NULL`.
#'
#' @details
#' `fusionInput()` executes a multi-stage data-preparation pipeline:
#'
#' * **Workspace Verification:** Ensures the active R session working directory is
#'   located within a valid `fusionData` repository root.
#' * **Microdata Harmonization:** Executes \code{harmonize()} using the matching donor-ACS
#'   harmonization mapping script (e.g., `RECS_2015__ACS_2015.R`).
#' * **Spatial Imputation:** Calls \code{imputeLocation()} to statistically assign
#'   PUMA-level geographic identifiers to donor survey observations.
#' * **Predictor Quality Screening:** Evaluates overlapping distribution similarity between
#'   donor and recipient predictor variables using weighted similarity metrics. Predictors with
#'   similarity scores below 0.80 are automatically dropped from model training to prevent
#'   distributional bias.
#' * **Numeric Feature Scaling:** Transforms continuous predictor variables using robust
#'   weighted Z-scores when necessary via `scaleNumeric()`.
#' * **Compressed File Output:** Writes donor (`*_donor.fst`) and recipient (`*_recipient.fst`)
#'   datasets utilizing maximum `fst` compression (`compress = 100`).
#'
#' @section Directory Structure & Outputs:
#' Output files are stored in structured paths based on execution mode:
#' \itemize{
#'   \item **Test Mode:** `fusion_/[DONOR_NAME]/[DONOR_VINTAGE]/[ACS_YEAR]/input/[DATE]/`
#'   \item **Production Mode:** `fusion/[DONOR_NAME]/[DONOR_VINTAGE]/[ACS_YEAR]/input/[DATE]/`
#' }
#' Each run creates three files in the target directory:
#' \enumerate{
#'   \item `[DONOR]_[ACS_YEAR]_[TYPE]_donor.fst`: Scaled, harmonized donor training microdata.
#'   \item `[DONOR]_[ACS_YEAR]_[TYPE]_recipient.fst`: Scaled, harmonized ACS prediction microdata.
#'   \item `[DONOR]_[ACS_YEAR]_[TYPE]_inputlog.txt`: Execution log containing system details,
#'         arguments, predictor similarity scores, and timing summaries.
#' }
#'
#' @return Invisibly returns a character string containing the absolute file path to the
#' generated `/input` directory.
#'
#' @seealso \code{\link{harmonize}}, \code{\link{imputeLocation}}
#'
#' @examples
#' \dontrun{
#' # Run input generation in fast test mode for RECS 2015 and ACS 2015
#' input_dir <- fusionInput(
#'   donor = "RECS_2015",
#'   acs_year = 2015,
#'   respondent = "household",
#'   test_mode = TRUE,
#'   ncores = 2
#' )
#'
#' # Inspect generated fst and log files
#' list.files(input_dir)
#' }
#'
#' @export

#---------------------------

# TESTING

# library(tidyverse)
# library(data.table)
# source("R/utils.R")

# RECS
# donor = "RECS_2024"
# respondent = "household"
# acs_year = 2019
# note = NULL
# test_mode = TRUE
# ncores = 2

# AHS 2023 test
# donor = "AHS_2023"
# respondent = "household"
# acs_year = 2023
# note = NULL
# test_mode = TRUE
# ncores = getOption("fusionData.cores")

#-----

fusionInput <- function(donor,
                        acs_year,
                        respondent,
                        test_mode = TRUE,
                        ncores = getOption("fusionData.cores"),
                        note = NULL
) {

  # Standardize respondent level identifier ("H" for Household, "P" for Person)
  rtype <- substring(toupper(respondent), 1, 1)
  respondent <- ifelse(rtype == "H", "household", "person")

  # Validate function parameter formats and constraints
  stopifnot({
    is.character(donor)
    acs_year >= 2005 & acs_year %% 1 == 0
    rtype %in% c("H", "P")
    is.null(note) | is.character(note)
    is.logical(test_mode)
    ncores > 0 & ncores %% 1 == 0
  })

  # Record pipeline execution start timestamp
  tstart <- Sys.time()

  # Set multi-threading thread limit for fst read/write operations
  fst::threads_fst(ncores)

  # Redirect output stream to a temporary log file while mirroring to console
  log.temp <- tempfile()
  log.txt <- file(log.temp, open = "wt")
  sink(log.txt, split = TRUE, type = "output")

  # Ensure working directory is situated inside the fusionData repository hierarchy
  input <- full.path(getwd())
  b <- strsplit(input, .Platform$file.sep, fixed = TRUE)[[1]]
  i <- which(b == "fusionData")
  if (length(i) == 0) stop("'/fusionData' is not part of the working directory's normalized path; this is required.")
  stub <- paste(b[1:i], collapse = .Platform$file.sep)

  # Record run header metadata and environment specifications
  tstamp <- as.POSIXct(Sys.time(), tz = "UTC")
  cli::cat_line("==================================================================")
  cli::cat_line("fusionInput() RUN INFORMATION")
  cli::cat_line("==================================================================")
  cli::cat_line("Timestamp: ", format(tstamp, usetz = TRUE))
  cli::cat_line("R Version: ", R.version.string)
  cli::cat_line("Platform:  ", R.Version()$platform)
  cli::cat_line("Package:   fusionData v", as.character(utils::packageVersion("fusionData")))

  # Log non-default parameter arguments
  cli::cat_line("Function Call:")
  print(match.call.defaults(exclude = if (is.null(note)) NULL else "note"))
  cli::cat_line()

  # Report current execution mode
  mode_text <- ifelse(test_mode, "TEST", "PRODUCTION")
  cli::cat_line("Execution Mode: ", mode_text)
  cli::cat_line()

  # Construct absolute output directory path based on mode, donor, vintage, and execution date
  donor <- toupper(donor)
  dir <- file.path(stub, ifelse(test_mode, "fusion_", "fusion"), sub("_", .Platform$file.sep, donor), acs_year, "input", as.Date(tstamp))
  cli::cat_line("Fusion INPUT Directory:")
  cli::cat_line("  ", dir)
  cli::cat_line()

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Log optional user comment if supplied
  if (!is.null(note)) {
    cli::cat_line("User-Supplied Note:")
    cli::cat_line("  ", note)
    cli::cat_line()
  }

  # Form base file path prefix for output artifacts
  stub <- file.path(dir, paste(donor, acs_year, rtype, sep = "_"))

  # Stage 1: Microdata Harmonization
  cli::cat_line("------------------------------------------------------------------")
  cli::cat_line("STEP 1: HARMONIZE DONOR AND RECIPIENT MICRODATA")
  cli::cat_line("------------------------------------------------------------------")

  harmonized <- harmonize(harmony.file = paste0(donor, "__ACS_", acs_year, ".R"),
                          respondent = respondent,
                          output = "both",
                          ncores = ncores)

  # Stage 2: Spatial Imputation
  cli::cat_line("------------------------------------------------------------------")
  cli::cat_line("STEP 2: IMPUTE LOCATION (PUMA) OF DONOR RESPONDENTS")
  cli::cat_line("------------------------------------------------------------------")

  location.data <- imputeLocation(harmonized = harmonized, ncores = ncores)

  # Merge harmonized features with imputed spatial attributes into aligned data tables
  key.vars <- intersect(names(location.data[[1]]), c('hid', 'pid', 'state', 'puma00', 'puma10', 'puma20'))

  D <- harmonized[[1]] %>%
    merge(location.data[[1]]) %>%
    select(any_of(key.vars), weight, everything()) %>%
    as.data.table(key = key.vars)

  R <- harmonized[[2]] %>%
    merge(location.data[[2]], by = key.vars) %>%
    select(any_of(key.vars), weight, any_of(names(D))) %>%
    as.data.table(key = key.vars)

  data <- list(donor = D, recipient = R)
  rm(D, R, harmonized, location.data)

  # Stage 3: Predictor Variable Screening & Scaling
  cli::cat_line("------------------------------------------------------------------")
  cli::cat_line("STEP 3: CHECK HARMONIZED PREDICTOR VARIABLES")
  cli::cat_line("------------------------------------------------------------------")

  cvars <- data[[1]] %>%
    select(-any_of(key(.)), -weight) %>%
    select_if(is.factor) %>%
    names()
  nvars <- setdiff(names(data[[1]]), c(key(data[[1]]), 'weight', cvars))

  # Compute distributional similarity for categorical predictors
  csim <- sapply(cvars, function(v) {
    categorical_similarity(x = data[[1]][[v]],
                           y = data[[2]][[v]],
                           wx = data[[1]]$weight,
                           wy = data[[2]]$weight)
  })

  # Apply robust Z-score scaling to numeric features and compute similarity
  nsim <- setNames(vector("double", length(nvars)), nvars)
  for (v in nvars) {
    out <- scaleNumeric(x1 = data[[1]][[v]],
                        x2 = data[[2]][[v]],
                        w1 = data[[1]]$weight,
                        w2 = data[[2]]$weight)
    set(data[[1]], j = v, value = out[[1]])
    set(data[[2]], j = v, value = out[[2]])
    nsim[v] <- out[[3]]
  }

  # Combine and report similarity scores across all candidate features
  sim <- c(csim, nsim) %>%
    tibble::enframe(name = "Harmonized variable", value = "Similarity score") %>%
    mutate(`Similarity score` = round(`Similarity score`, 3)) %>%
    arrange(`Similarity score`)

  cli::cat_line("Similarity scores for ", nrow(sim), " harmonized predictor variables:")
  print(sim, n = Inf)

  # Filter out predictor variables with similarity below the 0.80 quality threshold
  drop <- sim %>%
    filter(`Similarity score` < 0.8) %>%
    pull(`Harmonized variable`)

  if (length(drop) == 0) {
    cli::cat_line("\nRetaining all categorical harmonized variables")
  } else {
    cli::cat_line("\nRemoved the following harmonized predictor variables (similarity score below 0.8):")
    for (d in drop) cli::cat_line("  * ", d)
    data[[1]] <- select(data[[1]], -all_of(drop))
    data[[2]] <- select(data[[2]], -all_of(drop))
  }

  # Count retained predictor categories
  harm.vars <- grep("__", names(data[[1]]), fixed = TRUE, value = TRUE)
  loc.vars <- grep("loc..", names(data[[1]]), fixed = TRUE, value = TRUE)
  cli::cat_line("\nUtilizing ", length(harm.vars), " harmonized respondent-level predictor", if (length(harm.vars) != 1) "s" else "",
                " and ", length(loc.vars), " location predictor", if (length(loc.vars) != 1) "s" else "")
  cli::cat_line()

  # Stage 4: File Output Generation
  cli::cat_line("------------------------------------------------------------------")
  cli::cat_line("STEP 4: WRITE FUSION INPUT FILES TO DISK")
  cli::cat_line("------------------------------------------------------------------")

  # Export compressed donor training microdata
  cli::cat_line("Writing harmonized donor microdata...")
  dfile <- paste(stub, "donor.fst", sep = "_")
  n0 <- nrow(data[[1]])
  if (test_mode) data[[1]] <- slice(data[[1]], 1:min(10e3, n0))

  data[[1]] %>%
    fst::write_fst(path = dfile, compress = 100)

  fsize <- signif(file.size(dfile) / 1e6, 3)
  data[[1]] <- NA
  invisible(gc())

  cli::cat_line("Results saved to: ", basename(dfile), " (", fsize, " MB)")
  if (test_mode) cli::cat_line("  - TEST mode: Saved partial donor data.")

  # Export compressed ACS recipient prediction microdata
  cli::cat_line("\nWriting harmonized ACS microdata...")
  rfile <- paste(stub, "recipient.fst", sep = "_")
  n0 <- nrow(data[[2]])
  if (test_mode) data[[2]] <- slice(data[[2]], 1:min(10e3, n0))

  data[[2]] %>%
    select(-weight) %>%  # Exclude weight column (re-attached during analysis phase via master ACS microdata)
    fst::write_fst(path = rfile, compress = 100)

  fsize <- signif(file.size(rfile) / 1e6, 3)

  invisible(gc())

  cli::cat_line("Results saved to: ", basename(rfile), " (", fsize, " MB)")
  if (test_mode) cli::cat_line("  - TEST mode: Saved partial recipient data.")

  # Completion Summary & Log Archiving
  cli::cat_line()
  cli::cat_line("==================================================================")
  cli::cat_line("fusionInput() IS FINISHED!")
  cli::cat_line("==================================================================")

  tout <- difftime(Sys.time(), tstart)
  cli::cat_line("\nTotal processing time: ", signif(as.numeric(tout), 3), " ", attr(tout, 'units'))

  # Finalize text log copy to output directory
  log.path <- file.path(dir, paste(donor, acs_year, rtype, "inputlog.txt", sep = "_"))
  cli::cat_line("Log file saved to:\n  ", log.path)
  cli::cat_line()

  sink(type = "output")
  close(log.txt)
  invisible(file.copy(from = log.temp, to = log.path, overwrite = TRUE))

  # Invisibly return output directory path
  return(invisible(dir))

}
