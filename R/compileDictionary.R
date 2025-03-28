#' Compile universal survey dictionary
#'
#' @description
#' Compiles all of the individual survey data dictionaries in \code{/survey-processed} into two tibbles that are saved to disk for use by fusionData package as well as \link{universe} and \link{harmony} Shiny apps.
#'
#' @return Saves \code{dictionary.rda} and \code{surveys.rda} data frames to disk in \code{/data}, \code{/universe/www}, and \code{/harmony/www}.
#'
#' @examples
#' compileDictionary()
#'
#' @export

compileDictionary <- function() {

  # Extract data from /survey-processed and build codebook
  files <- list.files(path = "survey-processed", pattern = "_._dictionary.rds", recursive = TRUE, full.names = TRUE)

  # Compile dictionary
  dictionary <- files %>%
    map_dfr(readRDS) %>%
    mutate(respondent = ifelse(substring(tolower(respondent), 1, 1) == "h", "Household", "Person")) %>%
    rename_with(stringr::str_to_title)

  # Get processed mirodata file sizes on disk (MB)
  # This incudes .processed.fst and (if present) custom.fst files
  fsize.processed <- file.size(gsub("dictionary.rds", "processed.fst", files))
  fsize.custom <- file.size(gsub("dictionary.rds", "custom.fst", files))
  fsize <- prettyNum(rowSums(cbind(fsize.processed, fsize.custom), na.rm = TRUE) / 1e6, format = "g", digits = 3)

  # Create summary of available surveys
  surveys <- dictionary %>%
    group_by(Survey, Vintage, Respondent) %>%
    summarize(`Sample size` = format(max(N), big.mark = ","), `No. of variables` = format(n(), big.mark = ","), .groups = 'drop') %>%
    mutate(`Size on disk (MB)` = fsize)

  # Remove variable 'N' from 'dictionary'
  dictionary$N <- NULL

  # Save compiled dictionary files to /data
  usethis::use_data(dictionary, overwrite = TRUE)
  usethis::use_data(surveys, overwrite = TRUE)

  # ALSO save copies to /harmony Shiny app
  cat("Saving 'dictionary' and 'surveys' to /harmony/www\n")
  if (!dir.exists("harmony/www")) dir.create("harmony/www")
  save(dictionary, file = "harmony/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "harmony/www/surveys.rda", compress = TRUE)

  # ALSO save copies to /universe Shiny app
  cat("Saving 'dictionary' and 'surveys' to /universe/www\n")
  if (!dir.exists("universe/www")) dir.create("universe/www")
  save(dictionary, file = "universe/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "universe/www/surveys.rda", compress = TRUE)

  # Print summary of data outputs
  # message("dictionary.rda dimensions: ", paste(dim(dictionary), collapse = " x "))
  # message("surveys.rda dimensions: ", paste(dim(surveys), collapse = " x "))

}
