#' Generate /input files needed for fusion
#'
#' @description
#' Handles all operations needed to generate /input files from successfully ingested and harmonized donor survey microdata. Also (optionally) uploads resulting local /input data files to correct location in remote storage (via \code{\link{uploadFiles}}).
#' \cr\cr NOTE: Argument \code{test_mode} is \code{TRUE} by default. It must be explicitly set to \code{FALSE} to create (or possibly overwrite) production data in local /fusion directory.
#'
#' @param donor Character. Donor survey identifier (e.g. `"RECS_2015"`).
#' @param recipient Character. Recipient (ACS) survey identifier (e.g. `"ACS_2015"`).
#' @param respondent Character. Desired respondent level of microdata. Either `"household"` or `"person"`.
#' @param fuse Character or list. Names of donor variables to be fused to recipient. If \code{fuse} is a list, each entry is a character vector possibly indicating multiple variables to fuse as a block. The order of the \code{fuse} variables does not matter, since \code{\link[fusionModel]{prepXY}} is used internally to determine a plausible fusion sequence. If NULL (default), an attempt is made to return all donor variables not used in predictor harmonization process.
#' @param force Character. Pre-specified subset of potential predictor variables to "force" as included predictors. These variables are also used within \code{\link{fusionOutput}} to create validation subsets. We generally select the variables that best reflect the following socioeconomic and geographic concepts: income; race/ethnicity; education; household size; housing tenure; and the highest-resolution location variable for which the donor survey is thought to be representative.
#' @param notes Character. Optional notes supplied by user. These are added to the log file for reference.
#' @param test_mode Logical. If \code{TRUE} (default), function uses the local "/fusion_" sub-directory (creating it if necessary). Only when \code{test_mode = FALSE} is it possible to overwrite production data in /fusion (no underscore).
#' @param ncores Integer. Number of physical CPU cores used for parallel computation.
#'
#' @details The following steps:
#'
#' @return Saves resulting /input data files to appropriate local directory and (optionally) to remote Google Drive storage. Also saves a .txt log file alongside data files that records console output from \code{fusionInput()}.
#'
#' @export

#---------------------------
#---------------------------

# TESTING

# RECS
# fusionInput(
#   donor = "RECS_2015",
#   recipient = "ACS_2015",
#   ncores = 2,
#   respondent = "household",
#   fuse = c("btung", "btuel", "cooltype","scalee","scaleg",'scaleb','noheatng', "btufo", "btulp",
#                 "noacbroke","noacel","noheatel",'noheatbroke','noheatbulk','coldma','hotma'),
#   force = c("moneypy", "householder_race", "education", "nhsldmem", "kownrent", "recs_division")
# )

# CEI
# fusionInput(
#   donor = "CEI_2015-2019",
#   recipient = "ACS_2019",
#   ncores = 2,
#   respondent = "household",
#   fuse = c("cloftw", "jwlbg", "educ"),  # Or fuse = NULL for full processing
#   force = c("fincbtxm", "ref_race", "educ_ref", "fam_size", "cutenure", "division")
# )

# RECS
# donor = "RECS_2015"
# recipient = "ACS_2015"
# ncores = 2
# respondent = "household"
# fuse = c("btung", "btuel", "cooltype","scalee","scaleg",'scaleb','noheatng', "btufo", "btulp",
#          "noacbroke","noacel","noheatel",'noheatbroke','noheatbulk','coldma','hotma')
# force = c("moneypy", "householder_race", "education", "nhsldmem", "kownrent", "recs_division")
#
# CEI
# donor = "CEI_2015-2019"
# recipient = "ACS_2019"
# ncores = 2
# respondent = "household"
# fuse = c("cloftw", "jwlbg", "educ")  # Or fuse = NULL for full processing
# #fuse = NULL
# force <- c("fincbtxm", "ref_race", "educ_ref", "fam_size", "cutenure", "division")
# notes = NULL
# upload = FALSE

#-----

fusionInput <- function(donor,
                        recipient,
                        respondent,
                        fuse = NULL,
                        force = NULL,
                        notes = NULL,
                        test_mode = TRUE,
                        ncores = getOption("fusionData.cores")) {

  # 'ask' for user input from console
  ask <- interactive()

  # Respondent identifier ("H" or "P")
  rid <- substring(toupper(respondent), 1, 1)

  # Check input arguments
  stopifnot({
    is.character(donor)
    is.character(recipient)
    rid %in% c("H", "P")
    is.null(fuse) | is.character(fuse) | is.list(fuse)
    is.null(force) | is.character(force)
    is.null(notes) | is.character(notes)
    is.logical(test_mode)
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

  # Directory in /fusion where results will be saved
  # This is automatically constructed from 'donor' and 'recipient', assuming recipient is ACS-based
  acs.vintage <- substring(recipient, 5)
  stub <- paste(ifelse(test_mode, "fusion_", "fusion"), sub("_", "/", donor), acs.vintage, "input", sep = "/")  # TEST with fusion_

  #-----

  # Report initial messages to console and log

  cat(format(tstart, usetz = TRUE), "\n")
  cat(R.version.string, "\n")
  cat("Platform:", R.Version()$platform, "\n")
  cat("fusionData v", as.character(utils::packageVersion("fusionData")), "\n", sep = "")
  cat("fusionModel v", as.character(utils::packageVersion("fusionModel")), "\n\n", sep = "")

  # Print the original function arguments
  # Excludes 'notes', if present, since it is printed separately to log, below
  print(match.call.defaults(exclude = if (is.null(notes)) NULL else "notes"))
  cat("\n")

  # Write 'notes' argument to log file (and console), if requested
  if (!is.null(notes)) cat("User-supplied notes:\n", notes, "\n")

  # Create the /input directory, if necessary
  dir <- normalizePath(stub, mustWork = FALSE)
  if (dir.exists(dir)) {
    cat("The local /input directory already exists:\n")
  } else {
    dir.create(dir, recursive = TRUE)
    cat("The local /input directory was created:\n")
  }
  cat(dir, "\n")

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
  if (is.null(fuse)) stop("'fuse' is not specified or constructed on-the-fly")
  data <- fusionData::assemble(prep,
                               fusion.variables = unlist(fuse),
                               spatial.datasets = "all",
                               window = 2)

  # If NULL, update 'fuse' to reflect variables returned by 'assemble'
  if (is.null(fuse)) fuse <- attr(data, "fusion.vars")

  rm(prep)

  #-----

  cat("\n|=== Check for custom .R scripts ===|\n\n")

  # Check if there are any "(0X)" custom .R scripts that needs to be run
  cus <- list.files(dir, pattern = "(\\d{2}).*\\.R$", full.names = TRUE)
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
    print(check)

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
      print(loc.drop)
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
  prep <- fusionModel::prepXY(data = data[[1]],
                              y = fuse,
                              x = pred.vars,
                              weight = "weight",
                              xforce = xforce,
                              fraction = min(1, 50e3 / nrow(data[[1]])),
                              cor_thresh = 0.025,
                              lasso_thresh = 0.975,
                              xmax = 100,
                              cores = ncores)

  # Save output from prepXY()
  # saveRDS(prep, file = file.path(td, paste(donor, acs.vintage, rid, "prep.rds", sep = "_")))
  # cat("Results of prepXY() saved to temporary directory\n")

  # Save output from prepXY()
  xfile <- paste(stub, "prep.rds", sep = "_")
  saveRDS(prep, file = xfile)
  fsize <- signif(file.size(xfile) / 1e6, 3)
  cat("\nResults of prepXY() saved to:", paste0(basename(xfile), " (", fsize, " MB)"), "\n")

  #-----

  cat("\n|=== Write training and prediction datasets to disk ===|\n\n")

  # Update 'pred.vars' to the subset of predictors retained in 'prep'
  pred.vars <- attr(prep, "xpredictors")

  # Save training data to disk
  cat("Writing training dataset...\n")
  tfile <- paste(stub, "train.fst", sep = "_")
  data[[1]] %>%
    select(one_of(c("weight", unlist(prep$y), pred.vars))) %>%
    fst::write_fst(path = tfile, compress = 100)
  fsize <- signif(file.size(tfile) / 1e6, 3)
  data[[1]] <- NA
  invisible(gc())
  cat("Training dataset saved to:", paste0(basename(tfile), " (", fsize, " MB)"), "\n")

  # Save prediction data to disk
  cat("\nWriting prediction dataset...\n")
  pfile <- paste(stub, "predict.fst", sep = "_")
  data[[2]] %>%
    select(one_of(pred.vars)) %>%
    fst::write_fst(path = pfile, compress = 100)
  fsize <- signif(file.size(pfile) / 1e6, 3)
  rm(data)
  invisible(gc())
  cat("Prediction dataset saved to:", paste0(basename(pfile), " (", fsize, " MB)"), "\n")

  #-----

  cat("\n|=== Upload /input files to Google Drive ===|\n\n")
  if (ask) uploadFiles(files = c(xfile, tfile, pfile), ask = TRUE)

  #-----

  cat("\n|=== fusionInput() is finished! ===|\n\n")

  # Report processing time
  tout <- difftime(Sys.time(), tstart)
  cat("\nfusionInput() total processing time:", signif(as.numeric(tout), 3), attr(tout, "units"), "\n", sep = " ")

  # Finish logging and copy log file to /input
  log.path <- file.path(dir, paste(donor, acs.vintage, rid, "inputlog.txt", sep = "_"))
  cat("\nfusionInput() log file saved to:\n", log.path)
  sink(type = "output")
  close(log.txt)
  invisible(file.copy(from = log.temp, to = log.path, overwrite = TRUE))

}
