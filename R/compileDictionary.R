#' Compile Universal Survey Data Dictionary
#'
#' @description
#' Aggregates individual survey metadata codebooks stored in `survey-processed/`
#' into a single, standardized data dictionary (`dictionary`) and a high-level
#' survey metadata summary (`surveys`).
#'
#' @details
#' This function forms part of the **Document** step in the `fusionData` package
#' workflow. It scans all recursive survey metadata files (`*_dictionary.rds`) created
#' during survey ingest, standardizes respondent types, calculates microdata disk
#' footprints (including `.processed.fst` and optional `custom.fst` files), and
#' outputs unified data objects needed by the package and embedded Shiny applications
#' (`universe` and `harmony`).
#'
#' @section Directory Requirement:
#' **Important:** This function must be executed with your R working directory set to
#' the root of the local `fusionData` project folder (e.g., `setwd("path/to/fusionData")`).
#' It relies on relative directory paths (`survey-processed/`, `harmony/www/`, and `universe/www/`).
#'
#' @section Workflow Note:
#' Because `compileDictionary()` updates datasets stored in the package's `data/` directory,
#' you must rebuild or reinstall the package locally (e.g., using `fusionData::installPackage()`)
#' after running this function for the updated package datasets to take effect in your loaded session.
#'
#' @return Invisibly returns `NULL`. As a side effect, the function writes updated `.rda` files
#' containing the `dictionary` and `surveys` data frames to disk across three locations:
#' \itemize{
#'   \item `data/dictionary.rda` and `data/surveys.rda` (Package datasets)
#'   \item `harmony/www/dictionary.rda` and `harmony/www/surveys.rda` (Harmony Shiny app assets)
#'   \item `universe/www/dictionary.rda` and `universe/www/surveys.rda` (Universe Shiny app assets)
#' }
#'
#' @seealso \code{\link[usethis]{use_data}}
#'
#' @examples
#' \dontrun{
#' # Ensure working directory is set to the fusionData repository root
#' compileDictionary()
#'
#' # Reinstall local package binaries so updated data is recognized
#' installPackage()
#' }
#'
#' @export
compileDictionary <- function() {

  # Extract survey dictionary paths from processed metadata directory
  files <- list.files(path = "survey-processed", pattern = "_._dictionary.rds", recursive = TRUE, full.names = TRUE)

  # Read individual dictionaries, combine into master table, and standardize respondent labels
  dictionary <- files %>%
    map_dfr(readRDS) %>%
    mutate(respondent = ifelse(substring(tolower(respondent), 1, 1) == "h", "Household", "Person")) %>%
    rename_with(stringr::str_to_title)

  # Calculate combined size on disk (in MB) for processed and custom microdata files
  fsize.processed <- file.size(gsub("dictionary.rds", "processed.fst", files))
  fsize.custom <- file.size(gsub("dictionary.rds", "custom.fst", files))
  fsize <- prettyNum(rowSums(cbind(fsize.processed, fsize.custom), na.rm = TRUE) / 1e6, format = "g", digits = 3)

  # Summarize metadata across available survey-vintage-respondent combinations
  surveys <- dictionary %>%
    group_by(Survey, Vintage, Respondent) %>%
    summarize(`Sample size` = format(max(N), big.mark = ","), `No. of variables` = format(n(), big.mark = ","), .groups = 'drop') %>%
    mutate(`Size on disk (MB)` = fsize)

  # Drop raw sample size column from primary variable dictionary
  dictionary$N <- NULL

  # Save primary datasets to package data directory
  usethis::use_data(dictionary, overwrite = TRUE)
  usethis::use_data(surveys, overwrite = TRUE)

  # Mirror dictionary assets to the Harmony Shiny app web resources
  cli::cli_inform("Saving {.val dictionary} and {.val surveys} to {.path harmony/www}")
  if (!dir.exists("harmony/www")) dir.create("harmony/www")
  save(dictionary, file = "harmony/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "harmony/www/surveys.rda", compress = TRUE)

  # Mirror dictionary assets to the Universe Shiny app web resources
  cli::cli_inform("Saving {.val dictionary} and {.val surveys} to {.path universe/www}")
  if (!dir.exists("universe/www")) dir.create("universe/www")
  save(dictionary, file = "universe/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "universe/www/surveys.rda", compress = TRUE)

}
