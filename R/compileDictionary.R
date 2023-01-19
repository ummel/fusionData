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

  # Create summary of available surveys
  surveys <- dictionary %>%
    group_by(Survey, Vintage, Respondent) %>%
    summarize(`Sample size` = format(max(N), big.mark = ","), `No. of variables` = format(n(), big.mark = ","), .groups = 'drop')

  # Remove variable 'N'
  dictionary$N <- NULL

  # Save data outputs to disk: TWO locations
  # saveRDS(dictionary, "dictionary/dictionary.rds", compress = TRUE)
  # saveRDS(surveys, "dictionary/surveys.rds", compress = TRUE)
  #
  # saveRDS(dictionary, "harmony/dictionary.rds", compress = TRUE)
  # saveRDS(surveys, "harmony/surveys.rds", compress = TRUE)

  # Save compiled dictionary files to /data
  usethis::use_data(dictionary, overwrite = TRUE)
  usethis::use_data(surveys, overwrite = TRUE)

  # ALSO save copies to /universe Shiny app
  cat("Saving 'dictionary' and 'surveys' to /universe/www\n")
  save(dictionary, file = "universe/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "universe/www/surveys.rda", compress = TRUE)

  # ALSO save copies to /harmony Shiny app
  cat("Saving 'dictionary' and 'surveys' to /harmony/www\n")
  save(dictionary, file = "harmony/www/dictionary.rda", compress = TRUE)
  save(surveys, file = "harmony/www/surveys.rda", compress = TRUE)

  # Print summary of data outputs
  message("dictionary.rds dimensions: ", paste(dim(dictionary), collapse = " x "))
  message("surveys.rds dimensions: ", paste(dim(surveys), collapse = " x "))

}
