#' Update Local Package Datasets
#'
#' @description
#' Compiles updated datasets from source files in the local `/fusionData` file structure
#' and rebuilds the local fusionData package lazy-load database in-place.
#' This function should be executed following any addition, deletion, or modification to:
#' \itemize{
#'   \item Microdata dictionaries
#'   \item Spatial predictor data
#'   \item Data objects created or modified by code inside the \code{data-raw/} directory
#' }
#'
#' It provides a streamlined way to sync updated data directly into the local
#' fusionData package installation without requiring a full re-build from source.
#'
#' @section Side Effects on Disk:
#' Running this function executes several disk-writing operations across
#' the workspace and installed library:
#' \itemize{
#'   \item \strong{Local Package Data (\code{data/}):} Overwrites or generates
#'     \code{.rda} binary objects (\code{dictionary.rda}, \code{surveys.rda},
#'     \code{spatial.rda}, etc.) in the project's local \code{./data/} folder.
#'   \item \strong{Export Dependencies:} Writes updated dictionary and survey metadata
#'     to external application asset folders (\code{harmony/www} and \code{universe/www}).
#'   \item \strong{Spatial Cache Files:} Generates processed geospatial outputs,
#'     including serializing \code{geo-processed/geo_predictors.fst} to disk.
#'   \item \strong{Installed Package Database:} Modifies the lazy-load database
#'     files (\code{Rdata.rdb}, \code{Rdata.rdx}, and \code{Rdata.rds}) located in
#'     the installed package library directory (\code{system.file(package = "fusionData")}).
#' }
#'
#' @param compile_dictionary Logical. If \code{TRUE} (default), runs
#'   \code{\link{compileDictionary}()} to regenerate dictionary objects into \code{./data/}.
#' @param compile_spatial Logical. If \code{TRUE} (default), runs
#'   \code{\link{compileSpatial}()} to regenerate spatial objects into \code{./data/}.
#'
#' @return Invisibly returns a character vector of dataset names that were updated.
#' @export
#'
#' @examples
#' \dontrun{
#' # Update both dictionary and spatial datasets
#' update()
#'
#' # Skip spatial compilation and only update dictionary data
#' update(compile_spatial = FALSE)
#' }

update <- function(compile_dictionary = TRUE, compile_spatial = TRUE) {

  # 1. Run compilation routines if requested
  if (isTRUE(compile_dictionary)) {
    cli::cli_alert_info("Compiling dictionary files...")
    compileDictionary()
  }

  if (isTRUE(compile_spatial)) {
    cli::cli_alert_info("Compiling spatial files...")
    compileSpatial()
  }

  # 2. Verify target data directory exists and contains .rda files
  data_dir <- "data"
  if (!dir.exists(data_dir)) {
    cli::cli_abort("Directory {.path {data_dir}} does not exist.")
  }

  rda_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)
  if (length(rda_files) == 0) {
    cli::cli_abort("No {.path .rda} files found in {.path {data_dir}}.")
  }

  # 3. Locate the installed fusionData package on the system/HPC
  pkg_loc <- system.file(package = "fusionData")
  if (pkg_loc == "") {
    cli::cli_abort("Package {.pkg fusionData} is not installed in the active R library.")
  }

  # 4. Load all .rda objects into an isolated environment
  data_env <- new.env(parent = emptyenv())
  loaded_objs <- character()

  for (f in rda_files) {
    objs <- load(f, envir = data_env)
    loaded_objs <- c(loaded_objs, objs)
  }

  # 5. Unload active namespace to release file locks on database files
  if ("fusionData" %in% loadedNamespaces()) {
    pkgload::unload("fusionData")
  }

  # 6. Overwrite the lazy-load DB inside the installed package's data directory
  installed_data_dir <- file.path(pkg_loc, "data")
  if (!dir.exists(installed_data_dir)) {
    dir.create(installed_data_dir, recursive = TRUE)
  }

  db_base_path <- file.path(installed_data_dir, "Rdata")
  tools:::makeLazyLoadDB(data_env, db_base_path, compress = TRUE)

  # 7. Reload package cleanly without console output
  suppressPackageStartupMessages(
    suppressMessages(
      library(fusionData)
    )
  )

  unique_objs <- unique(loaded_objs)
  cli::cli_alert_success(
    "Successfully updated installed {.pkg fusionData} package data for: {.var {unique_objs}}"
  )

  invisible(unique_objs)
}
