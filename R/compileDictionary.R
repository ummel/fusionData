#' Compile universal survey dictionary
#'
#' @description
#' Compiles all of the individual survey data dictionaries in \code{/survey-processed} into a single tibble that is saved to disk for use by \link{dictionary} and \link{harmony} Shiny apps.
#'
#' @return Saves \code{dictionary.rds} and \code{surveys.rds} data frames to disk.
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
    mutate(respondent = ifelse(respondent == "H", "Household", "Person")) %>%
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

  save(dictionary, file = "data/dictionary.rda", compress = TRUE)
  save(surveys, file = "data/surveys.rda", compress = TRUE)

  # Print summary of data outputs
  message("dictionary.rds dimensions: ", paste(dim(dictionary), collapse = " x "))
  message("surveys.rds dimensions: ", paste(dim(surveys), collapse = " x "))

}
