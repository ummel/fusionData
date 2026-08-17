.onLoad <- function(libname, pkgname) {

  # Prevent styler cache message; See ?styler::caching
  options(styler.cache_root = "styler")

  # Create default option value for number of cores
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))  # Number of cores in Yale HPC environment
  if (is.na(ncores)) ncores <- max(1L, parallel::detectCores() - 1L)  # Generic fallback for local computing
  options(fusionData.cores = ncores)

}

.onAttach <- function(libname, pkgname) {

  # 1. Package Version and Remote Update Check
  current_ver <- utils::packageVersion("fusionData")

  # Always print package info first
  packageStartupMessage("fusionData v", current_ver, " | https://github.com/ummel/fusionData")

  # Direct raw URL check (bypasses GitHub API rate limits)
  remote_ver <- tryCatch({
    raw_url <- "https://raw.githubusercontent.com/ummel/fusionData/master/DESCRIPTION"

    # Set a strict 2-second timeout to prevent slowing down package load on slow connections
    opts <- options(timeout = 2)
    on.exit(options(opts), add = TRUE)

    desc_lines <- readLines(raw_url, warn = FALSE)
    ver_line <- grep("^Version:", desc_lines, value = TRUE)

    if (length(ver_line) > 0) {
      package_version(trimws(sub("^Version:", "", ver_line[1])))
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Alert based on remote version check
  if (!is.null(remote_ver)) {
    if (remote_ver > current_ver) {
      packageStartupMessage("A newer version (v", remote_ver, ") is available. Run devtools::install_github('ummel/fusionData') to upgrade.")
    } else {
      packageStartupMessage("You are using the latest version.")
    }
  }

  #---

  # 2. Yale HPC Automated Working Directory Configuration
  yale_target_dir <- "/gpfs/milgram/project/rao/shared/fusionACS/fusionData"

  # Check if running within Yale HPC environment and target path exists
  is_yale_hpc <- dir.exists("/gpfs/milgram") & Sys.getenv("SLURM_CLUSTER_NAME") == "milgram"

  if (is_yale_hpc && dir.exists(yale_target_dir)) {
    setwd(yale_target_dir)
    packageStartupMessage("Yale HPC Milgram detected. Working directory set to:\n", yale_target_dir)
  }

  # Warn if directory is still not /fusionData after attempting HPC adjustment
  if (basename(getwd()) != "fusionData") {
    packageStartupMessage("Warning: The fusionData package requires the working directory to be /fusionData")
  }

  # ---------------------------------------------------------------------------
  # 3. Check: 'harmony/www/dictionary.rda' vs 'survey-processed/*_*_dictionary.rds'
  # ---------------------------------------------------------------------------
  dict_rda <- "harmony/www/dictionary.rda"

  if (file.exists(dict_rda)) {
    dict_rda_time <- file.info(dict_rda)$mtime

    dict_rds_files <- list.files(
      path = "survey-processed",
      pattern = "_._dictionary.rds",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(dict_rds_files) > 0) {
      rds_mtimes <- file.info(dict_rds_files)$mtime
      if (any(rds_mtimes > dict_rda_time, na.rm = TRUE)) {
        packageStartupMessage("One or more survey dictionary files have been updated since compileDictionary() was last called.\n--> Run compileDictionary() to rebuild.")
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Check: 'geo-processed/geo_predictors.fst' vs 'geo-processed/*_processed.rds'
  # ---------------------------------------------------------------------------
  geo_fst <- "geo-processed/geo_predictors.fst"

  if (file.exists(geo_fst)) {
    geo_fst_time <- file.info(geo_fst)$mtime

    geo_rds_files <- list.files(
      path = "geo-processed",
      pattern = "_processed.rds$",
      recursive = TRUE,
      full.names = TRUE # Must be TRUE for file.info() to resolve paths correctly
    )

    if (length(geo_rds_files) > 0) {
      geo_rds_mtimes <- file.info(geo_rds_files)$mtime
      if (any(geo_rds_mtimes > geo_fst_time, na.rm = TRUE)) {
        packageStartupMessage("One or more processed geo files have been updated since compileSpatial() was last called.\n--> Run compileSpatial() to rebuild.")
      }
    }
  }

}
