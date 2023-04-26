#' Generate input files needed for fusion
#'
#' @description
#' Handles all operations needed to generate \code{/input} files from successfully ingested and harmonized donor survey microdata. Optionally uploads resulting local \code{/input} data files to correct location in remote storage (via \code{\link{uploadFiles}}).
#'
#' NOTE: Argument \code{test_mode = TRUE} by default, which causes local "/fusion_" sub-directory to be used (creating it if necessary). This prevents any overwrite of production data in "/fusion" (no underscore) while in test mode.
#'
#' @param donor Character. Donor survey identifier (e.g. `"RECS_2015"`).
#' @param recipient Character. Recipient (ACS) survey identifier (e.g. `"ACS_2015"`).
#' @param respondent Character. Desired respondent level of microdata. Either `"household"` or `"person"`.
#' @param fuse Character or list. Names of donor variables to be fused to recipient. If \code{fuse} is a list, each entry is a character vector possibly indicating multiple variables to fuse as a block. The order of the \code{fuse} variables does not matter, since \code{\link[fusionModel]{prepXY}} is used internally to determine a plausible fusion sequence. If NULL (default), an attempt is made to return all donor variables not used in predictor harmonization process (it is preferable to specify explicitly, though).
#' @param force Character. Pre-specified subset of potential predictor variables to "force" as included predictors. These variables are also used within \code{\link{fusionOutput}} to create validation subsets. We generally select the variables that best reflect the following socioeconomic and geographic concepts: income; race/ethnicity; education; household size; housing tenure; and the highest-resolution location variable for which the donor survey is thought to be representative.
#' @param note Character. Optional note supplied by user. Inserted in the log file for reference.
#' @param agg_fun List. Optional override of default aggregation function for person-level `fuse` variables when `respondent = "household"`. Passed to \code{\link{assemble}} internally. See Details.
#' @param agg_adj List. Optional pre-aggregation adjustment code to apply to person-level `fuse` variables when `respondent = "household"`. Passed to \code{\link{assemble}} internally. See Details.
#' @param test_mode Logical. If \code{TRUE} (default), function uses the local "/fusion_" sub-directory (creating it if necessary). Only when \code{test_mode = FALSE} is it possible to overwrite production data in "/fusion" (no underscore).
#' @param ncores Integer. Number of physical CPU cores used for parallel computation.
#'
#' @details The function checks arguments and determines the file path to the appropriate \code{/input} directory (creating it if necessary), based on \code{donor}, \code{recipient}, \code{respondent}, and \code{test_mode}. It then executes the following steps:
#' 1. **Check for custom pre-processing script**. Looks for an *optional*, pre-existing .R script in \code{/input} starting with "(00)". This script can be used to inject custom code prior to any other operations. Most likely, the custom code is used to set or modify function arguments that cannot be specified manually at function call. If found, the .R file is \code{\link{source}}-d locally and code comments are printed to the console.
#' 1. **prepare() microdata**. \code{\link{prepare}} is called with sensible default values.
#' 1. **assemble() microdata**. \code{\link{assemble}} is called with sensible default values. Output from \code{\link{assemble}} consists of an object named \code{data} with the harmonized donor microdata in \code{data[[1]]} and harmonized ACS microdata in \code{data[[2]]}.
#' 1. **Check for custom .R scripts**. Looks for *optional*, pre-existing .R scripts in \code{/input} starting with "(01)", "(02)", etc. These scripts can be used to inject custom code prior to next step. Most likely, the custom code is used to add or remove non-standard variables from \code{data} or otherwise adjust the default harmonized microdata. The custom code must modify the \code{fuse} vector (or list) if changes to the initial function argument are desired. If found, the .R files are \code{\link{source}}-d locally and code comments are printed to the console.
#' 1. **Check categorical harmonized variables**. Computes the similarity of donor and ACS categorical harmonized variables by comparing proportions for the observable factor levels. Under the assumption that the donor and ACS sample the same underlying population, we expect the proportions to be fairly similar. Returns a "Similarity score" for each categorical harmonized variable, ranging from 0 to 1. Variables with scores below ~0.8 should probably be checked by the analyst (via \code{\link{harmony}}) to confirm that the harmonization strategy is valid. User is prompted in the console to indicate which variables should be ignored/dropped/removed, if any.
#' 1. **Check location variables**. Checks the number of factor levels in each location predictor variable and compares to the number of levels in the "representative" location variable passed via \code{force}. If a location variable has more levels than the representative one, it is flagged as a potential issue since it suggests the presence of a location variable with greater spatial resolution than the one known to be representative. User is prompted in the console to indicate if they would like to remove the flagged variable(s).
#' 1. **Check fusion and predictor variables**. The full set of fusion and potential predictor variables is determined. Summary information is printed to the console. User is prompted in the console to confirm that everything looks OK before proceeding.
#' 1. **Run fusionModel::prepXY()**. \code{\link[fusionModel]{prepXY}} is called with sensible default values. The output is written to the appropriate \code{/input} directory and noted in console. \code{\link[fusionModel]{prepXY}} argument \code{fraction} is automatically set to use 10% of donor observations or 50k rows (10k in test mode), whichever is higher. Sampling often has minimal effect on results but speeds up computation.
#' 1. **Write training and prediction datasets to disk**. The (donor) training and (ACS) prediction datasets are written to the appropriate \code{/input} directory as fully-compressed \code{\link[fst]{fst}} files. Output file names noted in console. In test mode, no more than 10k rows of each is written to disk (for speed). In this case, the expected production file size is printed to the console.
#' 1. **Upload /input files to Google Drive**. User is prompted in the console to confirm if they would like to upload resulting \code{/input} data files to the analogous location in the remote Google Drive storage.
#' 1. **fusionInput() is finished!** Upon completion, a log file named \code{"inputlog.txt"} is written to \code{/input} for reference.
#'
#' The user is prompted for console input (including asking about GDrive upload) *only* if \code{\link{interactive}} is \code{TRUE}. Otherwise, the steps proceed without user input.
#'
#' If `donor` refers to a survey with both household- and person-level microdata *and* `respondent = "household"` *and* `fuse` includes person-level variables, then we have a situation where person-level fusion variables need to be aggregated to household-level prior to fusion.
#' This is done automatically within \code{\link{assemble}}. In this scenario, person-level fusion variables are aggregated based on their class.
#' By default, numeric variables return the household total (sum), unordered factors return the level of the household's reference person, and ordered factors return the household's maximum level.
#'
#' The `agg_fun` argument can be used to override the default aggregation function for specific fusion variables. It can reference any function that takes a vector and returns a single value and includes a 'na.rm' argument. Two special, package-specific functions are also available, "ref" and "mode", that return the reference person value and modal value, respectively. These functions are comparatively slow, especially "mode". See Examples.
#'
#' The `agg_adj` argument can be used to adjust/modify a person-level fusion variable prior to aggregation. This may be necessary if the variable is defined or measured in a way that doe not allow for straightforward aggregation to the household level. `agg_adj` supplies named formulas to an internal \code{\link[dplyr]{mutate}} call, allowing for complex modifications. See Examples.
#'
#' Note in the Examples the use of the convenience utility function `if.else()`. It wraps \code{\link[dplyr]{if_else}} and can be used identically but preserves factor levels and ordering in the result if possible.
#'
#' @return Invisibly returns path to local directory where files were saved. Messages printed to console noting progress. Resulting \code{/input} data files are saved to appropriate local directory and (optionally) to remote Google Drive storage. Also saves a .txt log file alongside data files that records console output from \code{fusionInput}.
#'
#' @examples
#' # Since 'test_mode = TRUE' by default, this will affect files in local /fusion_ directory
#' dir <- fusionInput(donor = "RECS_2015",
#'                    recipient = "ACS_2015",
#'                    respondent = "household",
#'                    fuse = c("btung", "btuel", "cooltype"),
#'                    force = c("moneypy", "householder_race", "education", "nhsldmem", "kownrent", "recs_division"),
#'                    note = "Hello world. Reminder: running in test mode by default.")
#'
#' # List files in the /input directory
#' list.files(dir)
#'
#' # Complicated ASEC example using custom aggregation arguments
#' dir <- fusionInput(donor = "ASEC_2019",
#'                    recipient = "ACS_2019",
#'                    respondent = "household",
#'                    fuse = c("heatsub", "heatval", "kidcneed", "hipval", "spmwic", "spmmort"),
#'                    agg_adj = list(
#'                       hipval = ~if.else(duplicated(asec_2019_hid), 0, hipval),
#'                       kidcneed = ~if.else(kidcneed == "NIU: Over 14", "No", kidcneed),
#'                       spmwic = ~if.else(duplicated(data.table(asec_2019_hid, spmfamunit)), 0, spmwic)
#'                    ),
#'                    agg_fun = list(
#'                       spmwic = "mean",
#'                       kidcneed = "mode"
#'                    ))
#'
#' @export

#---------------------------

# TESTING

# RECS
# donor = "RECS_2015",
# recipient = "ACS_2015",
# respondent = "household",
# note = NULL
# upload = FALSE
# ncores = 2
# fuse = c("btung", "btuel", "cooltype","scalee","scaleg",'scaleb','noheatng', "btufo", "btulp", "noacbroke","noacel","noheatel",'noheatbroke','noheatbulk','coldma','hotma'),
# force = c("moneypy", "householder_race", "education", "nhsldmem", "kownrent", "recs_division")
#
# CEI
# donor = "CEI_2015-2019"
# recipient = "ACS_2019"
# respondent = "household"
# fuse = c("cloftw", "jwlbg", "educ")  # Or fuse = NULL for full processing
# force = c("fincbtxm", "ref_race", "educ_ref", "fam_size", "cutenure", "division")
# note = NULL
# upload = FALSE
# ncores = 2

# ASEC TEST
# test_mode = TRUE
# donor = "ASEC_2019"
# recipient = "ACS_2019"
# respondent = "household"
# fuse = c("heatsub", "heatval", "kidcneed", "hipval", "spmwic", "spmmort", "fedretir")
# force = NULL
# note = NULL
# ncores = getOption("fusionData.cores")
# agg_adj = list(
#   hipval = ~if.else(duplicated(asec_2019_hid), 0, hipval),
#   kidcneed = ~if.else(kidcneed == "NIU: Over 14", "No", kidcneed),
#   spmwic = ~if.else(duplicated(data.table(asec_2019_hid, spmfamunit)), 0, spmwic)
# )
# agg_fun = list(
#   spmwic = "mean",
#   kidcneed = "mode"
# )

#-----

fusionInput <- function(donor,
                        recipient,
                        respondent,
                        fuse = NULL,
                        force = NULL,
                        note = NULL,
                        agg_fun = NULL,
                        agg_adj = NULL,
                        test_mode = TRUE,
                        ncores = getOption("fusionData.cores")) {

  # 'ask' for user input from console
  ask <- interactive()

  # Respondent identifier ("H" or "P")
  rid <- substring(toupper(respondent), 1, 1)
  respondent = ifelse(rid == "H", "household", "person")
  donor <- toupper(donor)
  recipient <- toupper(recipient)

  # Check input arguments
  stopifnot({
    is.character(donor)
    is.character(recipient)
    rid %in% c("H", "P")
    is.null(fuse) | is.character(fuse) | is.list(fuse)
    is.null(force) | is.character(force)
    is.null(note) | is.character(note)
    is.logical(test_mode)
    all(names(agg_adj) %in% fuse) & all(sapply(agg_adj, inherits, what = "formula"))
    all(names(agg_fun) %in% fuse) & all(sapply(agg_fun, function(x) is.function(get(x))))
    ncores > 0 & ncores %% 1 == 0
  })

  # Start time
  tstart <- Sys.time()

  # Set cores for 'fst' package to use
  fst::threads_fst(ncores)

  # Load nullDelta() function from /data
  load("data/nullDelta.rda")

  # Create log file
  log.temp <- tempfile()
  log.txt <- file(log.temp, open = "wt")
  sink(log.txt, split = TRUE, type = "output")

  #-----

  # Check validity of the working directory path
  # Also check if "/fusionData" is part of the path, as this is required
  input <- full.path(getwd())
  b <- strsplit(input, .Platform$file.sep, fixed = TRUE)[[1]]
  i <- which(b == "fusionData")
  if (length(i) == 0) stop("'/fusionData' is not part of the working directory's normalized path; this is required.")
  stub <- paste(b[1:i], collapse = .Platform$file.sep)

  #-----

  # Report initial messages to console and log
  cat(format(tstart, usetz = TRUE), "\n")
  cat(R.version.string, "\n")
  cat("Platform:", R.Version()$platform, "\n")
  cat("fusionData v", as.character(utils::packageVersion("fusionData")), "\n", sep = "")
  cat("fusionModel v", as.character(utils::packageVersion("fusionModel")), "\n\n", sep = "")

  # Print the original function arguments
  # Excludes 'note', if present, since it is printed separately to log, below
  print(match.call.defaults(exclude = if (is.null(note)) NULL else "note"))
  cat("\n")

  # Print message indicating 'test_mode' value
  cat("fusionInput() is running in", ifelse(test_mode, "TEST", "PRODUCTION"), "mode.\n\n")

  # Write 'note' argument to log file (and console), if requested
  if (!is.null(note)) cat("User-supplied note:\n", note, "\n\n")

  # Directory in /fusionData where /input results will be saved
  # This is automatically constructed from 'donor' and 'recipient', assuming recipient is ACS-based
  acs.vintage <- substring(recipient, 5)
  dir <- file.path(stub, ifelse(test_mode, "fusion_", "fusion"), sub("_", .Platform$file.sep, donor), acs.vintage, rid, "input")
  cat("Result files will be saved to:\n", dir, "\n\n")
  if (dir.exists(dir)) {
    cat("The local /input directory already exists.\n")
  } else {
    dir.create(dir, recursive = TRUE)
    cat("The local /input directory was created.\n")
  }

  # Update 'stub' to include file name information
  stub <- file.path(dir, paste(donor, acs.vintage, rid, sep = "_"))

  #-----

  cat("\n|=== Check for custom pre-processing script ===|\n\n")

  # Check if there is a "(00)" custom .R script that needs to be run
  cus0 <- list.files(dir, pattern = "(00).*\\.R$", full.names = TRUE)
  if (length(cus0) == 1) {
    cat("Detected custom .R pre-processing script:\n")
    cat(basename(cus0), "\n")
    ok <- if (ask) readline("Do you want to execute this script? (Y/N) ") else "Y"
    if (toupper(substring(ok, 1, 1)) == "Y") {
      cat("source()-ing:", basename(cus0), "\n")
      cat("(Only comments are printed)\n\n")
      source(cus0, echo = FALSE, local = TRUE)
      cmts <- grep("^\\s*#", readLines(cus0), value = TRUE)  # Extract and print comments
      cat(cmts, sep = "\n")
    } else {
      cat("Script not executed.\n")
    }
  } else {
    cat("None found.\n")
  }

  #-----

  cat("\n|=== prepare() microdata ===|\n\n")

  # Prepare and assemble data inputs
  prep <- fusionData::prepare(donor = donor,
                              recipient = recipient,
                              respondent = respondent,
                              implicates = 1)

  #-----

  cat("\n|=== assemble() microdata ===|\n\n")

  # Specify fusion variables to be retained in harmonization results
  data <- fusionData::assemble(x = prep,
                               fusion.variables = unlist(fuse),
                               spatial.datasets = "all",
                               window = 2,
                               pca = NULL,
                               replicates = FALSE,
                               agg_adj = agg_adj,
                               agg_fun = agg_fun)

  # If NULL, update 'fuse' to reflect variables returned by 'assemble'
  if (is.null(fuse)) fuse <- attr(data, "fusion.vars")

  rm(prep)

  #-----

  cat("\n|=== Check for custom .R scripts ===|\n\n")

  # Check if there are any "(XX)" custom .R scripts that needs to be run
  cus <- list.files(dir, pattern = "^\\(\\d{2}\\).*\\.R$", full.names = TRUE)
  cus <- setdiff(cus, cus0)
  if (length(cus) > 0) {
    cat("Detected custom .R processing script(s):\n")
    cat(paste(basename(cus), collapse = "\n"), "\n")
    ok <- if (ask) readline("Do you want to execute these script(s)? (Y/N) ") else "Y"
    if (toupper(substring(ok, 1, 1)) == "Y") {
      for (r in cus) {
        cat("\nsource()-ing:", basename(r), "\n")
        cat("(Only comments are printed)\n\n")
        source(r, echo = FALSE, local = TRUE)
        cmts <- grep("^\\s*#", readLines(r), value = TRUE)  # Extract and print comments
        cat(cmts, sep = "\n")
      }
    } else {
      cat("Script(s) not executed.\n")
    }
  } else {
    cat("None found.\n")
  }

  #-----

  # Recompile the attribute vectors for harmonized, and location variables
  # This is done to ensure the attributes are compatible with 'data', in case custom processing altered things along the way
  temp <- intersect(names(data[[1]]), names(data[[2]]))
  temp <- setdiff(temp, "weight")
  attr(data, "harmonized.vars") <- grep("..", temp, fixed = TRUE, invert = TRUE, value = TRUE)
  attr(data, "location.vars") <- grep("^loc\\.\\.", temp, value = TRUE)

  #-----

  # Check/compare observed proportions in donor and recipient microdata for the categorical harmonized variables

  cat("\n|=== Check categorical harmonized variables ===|\n\n")

  # TO DO: Could wrap this into stand-alone function
  harm.vars <- attr(data, "harmonized.vars")
  fxvars <- data[[1]] %>%
    slice(1L) %>%
    select(all_of(harm.vars)) %>%
    select_if(is.factor) %>%
    names()

  if (length(fxvars) > 0) {

    d1 <- data[[1]] %>%
      select(weight, all_of(fxvars)) %>%
      mutate(weight = weight / sum(weight)) %>%
      as.data.table()

    d2 <- data[[2]] %>%
      select(weight, all_of(fxvars)) %>%
      mutate(weight = weight / sum(weight)) %>%
      as.data.table()

    check <- sapply(fxvars, function(v) {
      p1 <- d1[, list(w1 = sum(weight)), by = v]
      p2 <- d2[, list(w2 = sum(weight)), by = v]
      p <- merge(p1, p2, by = v)
      delta <- sum(abs(p$w2 - p$w1)) / 2
      score <- 1 - delta / nullDelta(nrow(p))
      score <- round(score, 3)
      return(score)
    }) %>%
      tibble::enframe(name = "Harmonized variable", value = "Similarity score") %>%
      arrange(`Similarity score`)

    cat("Similarity scores for", length(fxvars), "categorical harmonized variables:\n")
    print(check, n = Inf)

    if (ask) {
      cat("\nShould any of the categorical harmonized variables be excluded?\n")
      answer <- readline(prompt = "Enter vector of row numbers to exclude or leave blank to retain all: ")
      answer <- eval(parse(text = answer))
      drop <- check[[1]][answer]
      if (length(drop) == 0) {
        cat("Retaining all categorical harmonized variables\n")
      } else {
        cat("Removing following harmonized predictor variables:\n", paste(drop, collapse = ", "), "\n")
        data[[1]] <- select(data[[1]], -all_of(drop))
        data[[2]] <- select(data[[2]], -all_of(drop))
      }
      setattr(data, "harmonized.vars", setdiff(harm.vars, drop))  # Update attribute in 'data'
    } else {
      cat("Retaining all categorical harmonized variables.\n")
    }
    rm(d1, d2, check)

  } else {
    cat("No categorical harmonized variables in microdata.\n")
  }

  #-----

  # Determine if any location variables should be dropped before proceeding

  cat("\n|=== Check location variables ===|\n\n")

  # Determine number of levels associated with each location variable
  loc.vars <- attr(data, "location.vars")
  loc.levels <- data[[1]] %>%
    select(all_of(loc.vars)) %>%
    map(levels) %>%
    lengths()

  # Detect and extract the representative location variable specified in 'force'
  rvar <- intersect(loc.vars, paste0("loc..", force))

  if (length(rvar) > 0) {

    # Are there location variables we may want to consider dropped?
    loc.drop <- loc.levels[loc.levels > max(loc.levels[rvar])]
    loc.drop <- tibble::enframe(loc.drop, name = "Location variable", "Number of levels")

    # Ask user to decide which location variables to drop (if any)
    if (nrow(loc.drop) > 0) {
      cat("The representative location variable '", rvar, "' has ", loc.levels[rvar], " levels.\n", sep = "")
      cat("The following location variables have been flagged for possible exclusion:\n")
      print(loc.drop, n = Inf)
      if (ask) {
        cat("\nShould any of these location variables be excluded?\n")
        answer <- readline(prompt = "Enter vector of row numbers to exclude or leave blank to retain all: ")
        answer <- eval(parse(text = answer))
        drop <- loc.drop[[1]][answer]
        if (length(drop) == 0) {
          cat("Retaining all location variables\n")
        } else {
          cat("Removing following location variables:\n", paste(drop, collapse = ", "), "\n")
          data[[1]] <- select(data[[1]], -all_of(drop))
          data[[2]] <- select(data[[2]], -all_of(drop))
        }
      } else {
        cat("Retaining all location variables.\n")
      }
    } else {
      cat("No issues detected.\n")
    }
  } else {
    cat("Check skipped: Did not detect any location variables in 'force'.\n")
  }

  #-----

  cat("\n|=== Check fusion and predictor variables ===|\n\n")

  # Identify the variables to be fused ('fuse')
  if (!all(unlist(fuse) %in% names(data[[1]]))) stop("Some of the 'fuse' variables are not in the donor microdata")
  cat("Identified ", length(unlist(fuse)), " fusion variables (", sum(lengths(fuse) > 1), " blocks):\n", sep = "")
  print(fuse)

  # Identify the predictor variables ('pred.vars')
  pred.vars <- setdiff(intersect(names(data[[1]]), names(data[[2]])), c("weight", fuse))
  cat("\nIdentified", length(attr(data, "harmonized.vars")), "harmonized variables and", length(pred.vars), "total predictors\n")

  # Identify the 'xforce' predictor variables
  if (!is.null(force)) {
    xforce <- lapply(force, function(x) {
      grep(paste0("^", x, "__"), attr(data, "harmonized.vars"), value = TRUE)
    }) %>%
      purrr::compact() %>%
      unlist() %>%
      c(rvar) # Add the previously identified representative geographic variable
    cat("\nIdentified", length(xforce), "predictors to force and use for validation:\n")
    print(xforce)
  } else {
    xforce <- NULL
  }

  # Make sure the variables look OK before proceeding
  ok <- if (ask) readline(prompt = "Does everything look OK at this point? (Y/N): ") else "Y"
  if (toupper(substring(ok, 1, 1)) != "Y") stop("Stopped at request of user.")

  #-----

  cat("\n|=== Run fusionModel::prepXY() ===|\n\n")

  # Determine fusion order and subset of 'pred.vars' to use with each fusion variable/block
  n0 <- nrow(data[[1]])
  pfrac <- min(1, ifelse(test_mode, 10e3, max(50e3, n0 * 0.1)) / n0)
  prep.xy <- fusionModel::prepXY(data = data[[1]],
                                 y = fuse,
                                 x = pred.vars,
                                 weight = "weight",
                                 cor_thresh = 0.025,
                                 lasso_thresh = 0.975,
                                 xmax = 100,
                                 xforce = xforce,
                                 fraction = pfrac,
                                 cores = ncores)

  # Save output from prepXY()
  xfile <- paste(stub, "prep.rds", sep = "_")
  saveRDS(prep.xy, file = xfile)
  fsize <- signif(file.size(xfile) / 1e6, 3)
  cat("\nResults of prepXY() saved to:", paste0(basename(xfile), " (", fsize, " MB)"), "\n")

  #-----

  cat("\n|=== Write training and prediction datasets to disk ===|\n\n")

  # Update 'pred.vars' to the subset of predictors retained in 'prep.xy'
  pred.vars <- attr(prep.xy, "xpredictors")

  # Save training data to disk
  cat("Writing training dataset...\n")
  tfile <- paste(stub, "train.fst", sep = "_")
  n0 <- nrow(data[[1]])
  if (test_mode) data[[1]] <- slice(data[[1]], 1:min(10e3, n0))  # Save only slice of full data in test mode.

  data[[1]] %>%
    select(one_of(c("weight", unlist(prep.xy$y), pred.vars))) %>%
    fst::write_fst(path = tfile, compress = 100)

  fsize <- signif(file.size(tfile) / 1e6, 3)
  fsize.true <- signif(fsize * n0 / nrow(data[[1]]), 3)
  data[[1]] <- NA
  invisible(gc())
  cat("Training dataset saved to:", paste0(basename(tfile), " (", fsize, " MB)"), "\n")
  if (test_mode & fsize.true > fsize) cat("Test mode: saved partial training data. Expected production file size is ~", fsize.true, "MB\n")

  # Save prediction data to disk
  cat("\nWriting prediction dataset...\n")
  pfile <- paste(stub, "predict.fst", sep = "_")
  n0 <- nrow(data[[2]])
  if (test_mode) data[[2]] <- slice(data[[2]], 1:min(10e3, n0))  # Save only slice of full data in test mode.

  data[[2]] %>%
    select(one_of(pred.vars)) %>%
    fst::write_fst(path = pfile, compress = 100)

  fsize <- signif(file.size(pfile) / 1e6, 3)
  fsize.true <- signif(fsize * n0 / nrow(data[[2]]), 3)
  rm(data)
  invisible(gc())
  cat("Prediction dataset saved to:", paste0(basename(pfile), " (", fsize, " MB)"), "\n")
  if (test_mode & fsize.true > fsize) cat("Test mode: Saved partial prediction data; expected production file size is ~", fsize.true, "MB\n" )

  #-----

  cat("\n|=== Upload /input files to Google Drive ===|\n\n")
  if (ask) {
    uploadFiles(files = c(xfile, tfile, pfile), ask = TRUE)
  } else {
    cat("Non-interactive session: skipping upload to Google Drive\n")
  }

  #-----

  cat("\n|=== fusionInput() is finished! ===|\n\n")

  # Report processing time
  tout <- difftime(Sys.time(), tstart)
  cat("Total processing time:", signif(as.numeric(tout), 3), attr(tout, "units"), "\n", sep = " ")

  # Finish logging and copy log file to /input
  log.path <- file.path(dir, paste(donor, acs.vintage, rid, "inputlog.txt", sep = "_"))
  cat("\nLog file saved to:\n", log.path)
  sink(type = "output")
  close(log.txt)
  invisible(file.copy(from = log.temp, to = log.path, overwrite = TRUE))

  # Return the /input path invisibly
  return(invisible(dir))

}
