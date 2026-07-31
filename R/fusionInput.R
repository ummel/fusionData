#' Generate Prepared Fusion Input Datasets from Harmonized Donor and ACS Microdata
#'
#' @description
#' Constructs and formats the aligned donor (training) and ACS recipient (prediction)
#' microdata datasets required for downstream statistical matching in the `fusionData`
#' workflow. `fusionInput()` orchestrates survey harmonization, geographic location
#' (PUMA) imputation, predictor distribution quality screening, and numeric feature
#' scaling before writing compressed binary `.fst` files to disk.
#'
#' @param donor Character. Identifier for the donor survey vintage (e.g., `"RECS_2015"`,
#'   `"AHS_2023"`). Must correspond to an existing harmonization file in `harmony/harmonies/`.
#' @param acs_year Integer. Year of ACS microdata serving as the recipient dataset (e.g., `2015`, `2023`).
#' @param respondent Character. Unit of observation; must be either `"household"` (or `"H"`)
#'   or `"person"` (or `"P"`).
#' @param test_mode Logical. If `TRUE` (default), outputs are written to a scratch directory
#'   (`fusion_/.../input/`) and datasets are truncated to ~10,000 observations for rapid testing.
#'   If `FALSE`, full-scale production files are written to `fusion/.../input/`.
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
#'   \item **Test Mode:** `fusion_/[DONOR]/[ACS_YEAR]/input/[DATE]/`
#'   \item **Production Mode:** `fusion/[DONOR]/[ACS_YEAR]/input/[DATE]/`
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
  cli::cli_inform(format(tstamp, usetz = TRUE))
  cli::cli_inform(R.version.string)
  cli::cli_inform(c("i" = "Platform: {.val {R.Version()$platform}}"))
  cli::cli_inform(c("i" = "Package: {.val fusionData v{as.character(utils::packageVersion('fusionData'))}}"))

  # Log non-default parameter arguments
  print(match.call.defaults(exclude = if (is.null(note)) NULL else "note"))
  cli::cli_inform("")

  # Report current execution mode
  mode_text <- ifelse(test_mode, "TEST", "PRODUCTION")
  cli::cli_inform(c("i" = "fusionInput() is running in {.strong {mode_text}} mode."))
  cli::cli_inform("")

  # Log optional user comment if supplied
  if (!is.null(note)) {
    cli::cli_inform(c("i" = "User-supplied note:\n{.val {note}}"))
    cli::cli_inform("")
  }

  # Construct absolute output directory path based on mode, donor, vintage, and execution date
  donor <- toupper(donor)
  dir <- file.path(stub, ifelse(test_mode, "fusion_", "fusion"), sub("_", .Platform$file.sep, donor), acs_year, "input", as.Date(tstamp))
  cli::cli_inform(c("i" = "Result files will be saved to:\n{.file {dir}}"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Form base file path prefix for output artifacts
  stub <- file.path(dir, paste(donor, acs_year, rtype, sep = "_"))

  # Stage: Microdata Harmonization
  cli::cli_h1("Harmonize donor and recipient microdata")

  harmonized <- harmonize(harmony.file = paste0(donor, "__ACS_", acs_year, ".R"),
                          respondent = respondent,
                          output = "both",
                          ncores = ncores)

  # Stage: Spatial Imputation
  cli::cli_h1("Impute location (PUMA) of donor respondents")

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

  # Stage: Predictor Variable Screening & Scaling
  cli::cli_h1("Check harmonized predictor variables")

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

  cli::cli_inform(c("i" = "Similarity scores for {.val {nrow(sim)}} harmonized predictor variables:"))
  print(sim, n = Inf)

  # Filter out predictor variables with similarity below the 0.80 quality threshold
  drop <- sim %>%
    filter(`Similarity score` < 0.8) %>%
    pull(`Harmonized variable`)

  if (length(drop) == 0) {
    cli::cli_alert_success("Retaining all categorical harmonized variables")
  } else {
    cli::cli_alert_warning("Removed the following harmonized predictor variables (similarity score below 0.8):")
    cli::cli_inform(c("*" = "{drop}"))
    data[[1]] <- select(data[[1]], -all_of(drop))
    data[[2]] <- select(data[[2]], -all_of(drop))
  }

  # Count retained predictor categories
  harm.vars <- grep("__", names(data[[1]]), fixed = TRUE, value = TRUE)
  loc.vars <- grep("loc..", names(data[[1]]), fixed = TRUE, value = TRUE)
  cli::cli_inform(c("v" = "Utilizing {.val {length(harm.vars)}} harmonized respondent-level predictor{?s} and {.val {length(loc.vars)}} location predictor{?s}"))

  # Stage: File Output Generation
  cli::cli_h1("Write fusion input files to disk")

  # Export compressed donor training microdata
  cli::cli_inform("Writing harmonized donor microdata...")
  dfile <- paste(stub, "donor.fst", sep = "_")
  n0 <- nrow(data[[1]])
  if (test_mode) data[[1]] <- slice(data[[1]], 1:min(10e3, n0))

  data[[1]] %>%
    fst::write_fst(path = dfile, compress = 100)

  fsize <- signif(file.size(dfile) / 1e6, 3)
  fsize.true <- signif(fsize * n0 / nrow(data[[1]]), 2)
  data[[1]] <- NA
  invisible(gc())

  cli::cli_alert_success("Harmonized donor microdata saved to: {.file {basename(dfile)}} ({.val {fsize}} MB)")
  if (test_mode & fsize.true > fsize) {
    cli::cli_alert_info("TEST mode: Saved partial donor data. Expected production file size is ~{.val {fsize.true}} MB")
  }

  # Export compressed ACS recipient prediction microdata
  cli::cli_inform("Writing harmonized ACS microdata...")
  rfile <- paste(stub, "recipient.fst", sep = "_")
  n0 <- nrow(data[[2]])
  if (test_mode) data[[2]] <- slice(data[[2]], 1:min(10e3, n0))

  data[[2]] %>%
    select(-weight) %>%  # Exclude weight column (re-attached during analysis phase via master ACS microdata)
    fst::write_fst(path = rfile, compress = 100)

  fsize <- signif(file.size(rfile) / 1e6, 3)
  fsize.true <- signif(fsize * n0 / nrow(data[[2]]), 3)
  invisible(gc())

  cli::cli_alert_success("Harmonized ACS microdata saved to: {.file {basename(rfile)}} ({.val {fsize}} MB)")
  if (test_mode & fsize.true > fsize) {
    cli::cli_alert_info("TEST mode: Saved partial recipient data. Expected production file size is ~{.val {fsize.true}} MB")
  }

  # Completion Summary & Log Archiving
  cli::cli_h1("fusionInput() is finished!")

  tout <- difftime(Sys.time(), tstart)
  cli::cli_alert_success("Total processing time: {.val {signif(as.numeric(tout), 3)}} {attr(tout, 'units')}")

  # Finalize text log copy to output directory
  log.path <- file.path(dir, paste(donor, acs_year, rtype, "inputlog.txt", sep = "_"))
  cli::cli_inform(c("i" = "Log file saved to:\n{.file {log.path}}"))

  sink(type = "output")
  close(log.txt)
  invisible(file.copy(from = log.temp, to = log.path, overwrite = TRUE))

  # Invisibly return output directory path
  return(invisible(dir))

}
