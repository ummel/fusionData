#' Download processed microdata
#'
#' @description
#' Since processed survey microdata files are generally too large to upload to Github or attach to the fusionData package, all of the processed microdata (.fst) files are stored on Google Drive. This function downloads requested survey microdata and places it in the appropriate local sub-directory of /fusionData. The full file path (e.g. intermediate directories) are created, if necessary, but no existing data on disk is altered or deleted.
#'
#' @param survey Character. Survey identifier, possibly including vintage and respondent type. See Details.
#'
#' @details \code{survey} must include a unique survey identifier. If just the identifier is provided (e.g. \code{survey = "RECS"}), then all available processed microdata across vintages and respondents is downloaded. Alternatively, can specify survey and vintage (e.g. \code{survey = "RECS_2015"}) or include additional respondent code (e.g. \code{survey = "RECS_2015_H"}) to limit download to more restrictive subsets. The special argument \code{survey = "all"} will download ALL available processed microdata across surveys.\cr\cr
#' The function will fail with an explanation if the current working directory is not set to \code{/fusionModel}; i.e. the project directory as pulled from Github.
#'
#' @return \code{\link[googledrive]{drive_download}} prints messages to console indicating which files were downloaded.
#'
#' @examples
#' getProcessedData(survey = "RECS_2015_H")
#'
#' @export

getSurveyProcessed <- function(survey) {

  if (basename(getwd()) != "fusionData") stop("The current directory -- see getwd() -- must be set to local /fusionData")

  # TO DO: Switch to silent auth eventually:
  # See: vignette("non-interactive-auth") in gargle package
  # https://gargle.r-lib.org/articles/get-api-credentials.html
  message("You may need to authorize the 'googledrive' package via your browser")
  message("Password: fusethis!")
  googledrive::drive_auth(email = "fusionacsdata@gmail.com")

  # TEST 'survey' values
  # survey <- "RECS"
  # survey <- "RECS_2015"
  # survey <- "RECS_2015_H"

  # REGEX expression used to search for files
  search <- if (survey == "all") {
    "^.*"
  } else {
    switch(as.character(nchar(survey) - nchar(gsub("_", "", survey))),
           `0` = paste0("^", survey, "_.*_."),
           `1` = paste0("^", survey, "_."),
           `2` = paste0("^", survey))
  }

  #-----

  # Identify data (.fst) files
  files.fst <- googledrive::drive_find(pattern = paste0(search, "_processed.fst$")) %>%
    googledrive::drive_reveal("path") %>%
    mutate(path = gsub("~/fusionData/", "", path))

  # Download data (.fst) files
  for (i in 1:nrow(files.fst)) {
    dir.create(dirname(files.fst[i, ]$path), recursive = TRUE, showWarnings = FALSE)
    googledrive::drive_download(file = files.fst[i, ],
                   path = files.fst[i, ]$path,
                   overwrite = TRUE,
                   verbose = TRUE)
  }

  #-----

  # NOT USED
  # Download dictionary (.rds) files, if requested
  # if (dictionary) {
  #   files.rds <- googledrive::drive_find(pattern = paste0(search, "_dictionary.rds$")) %>%
  #     googledrive::drive_reveal("path") %>%
  #     mutate(path = gsub("~/fusionData/", "", path))
  #   for (i in 1:nrow(files.rds)) {
  #     dir.create(dirname(files.rds[i, ]$path), recursive = TRUE, showWarnings = FALSE)
  #     googledrive::drive_download(file = files.rds[i, ],
  #                    path = files.rds[i, ]$path,
  #                    overwrite = TRUE,
  #                    verbose = TRUE)
  #   }
  # }

}
