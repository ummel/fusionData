#' Compile Survey Microdata Dictionaries
#'
#' @description
#' Aggregates individual survey dictionaries stored in \code{survey-processed/}
#' into a single, standardized data dictionary (\code{dictionary}) and a high-level
#' survey metadata summary (\code{surveys}).
#'
#' @details
#' This function scans all recursive survey dictionary files (\code{*_dictionary.rds}) created
#' during survey ingest, standardizes respondent types, calculates microdata disk
#' footprints (including \code{.processed.fst} and optional \code{custom.fst} files), and
#' outputs unified data objects needed by the embedded Shiny applications
#' (\code{universe} and \code{harmony}).
#'
#' \strong{Important:} This function must be executed with your R working directory set to
#' the root of the local \code{fusionData} project folder (e.g., \code{setwd("path/to/fusionData")}).
#' It relies on relative directory paths (\code{survey-processed/}, \code{harmony/www/}, and \code{universe/www/}).
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
  # use_data2(dictionary, overwrite = TRUE)
  # use_data2(surveys, overwrite = TRUE)

  # Mirror dictionary assets to the Harmony Shiny app web resources
  cli::cli_inform("Saving {.val dictionary} and {.val surveys} to {.path harmony/www}")
  save(dictionary, file = "harmony/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "harmony/www/surveys.rda", compress = TRUE)

  # Mirror dictionary assets to the Universe Shiny app web resources
  cli::cli_inform("Saving {.val dictionary} and {.val surveys} to {.path universe/www}")
  save(dictionary, file = "universe/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "universe/www/surveys.rda", compress = TRUE)

}
