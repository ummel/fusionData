# Specify package imports
#' @import DT
#' @import gt
#' @import rhandsontable
#' @import shinyWidgets
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import tibble
#' @import fst
#' @rawNamespace import(shinyjs, except = c(html, runExample, alert))
#' @rawNamespace import(matrixStats, except = c(count))
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(stats, except = c(filter, lag))
#' @rawNamespace import(data.table, except = c(first, last, between, transpose))
NULL

#-----

.onLoad <- function (libname, pkgname) {

  # Check that the current working directory is /fusionData
  if (basename(getwd()) != "fusionData") stop("The fusionData package requires the working directory to be /fusionData")

  # Create default option value for number of cores
  options(fusionData.cores = max(1L, parallel::detectCores() - 1L))

  # Authenticate 'fusionACSdata' GDrive access using locally-stored token
  # load("data/token.rda")
  # googledrive::drive_deauth()
  # googledrive::drive_auth(token = token)

  # NOTE: To generate/update the token, run this once in console
  # googledrive::drive_auth(email = "fusionacsdata@gmail.com")
  # token <- googledrive::drive_token()
  # save(token, file = "data/token.rda")

  #---

  # # Get timestamps for critical package files in /data, /man, and /R
  # temp <- utils::packageDescription("fusionData", fields = "Built")
  # pkg.date <- ifelse(is.na(temp), Inf, as.POSIXct(strsplit(temp, ";", fixed = TRUE)[[1]][3], tz = "UTC"))
  # pkg.files <- list.files(c("data", "man", "R"), recursive = FALSE, full.names = TRUE)
  # tstamp <- file.mtime(pkg.files)
  #
  # # Identify if there are local survey dictionary files more recent than the existing 'data/dictionary.rda' file
  # if (interactive()) {
  #
  #   dct.files <- list.files("survey-processed", pattern = "_dictionary.rds", recursive = TRUE, full.names = TRUE)
  #   mod.files <- dct.files[file.mtime(dct.files) > tstamp[pkg.files == "data/dictionary.rda"]]
  #
  #   # Ask user if they would like to update 'dictionary.rda'
  #   if (length(mod.files) > 0) {
  #     message("Detected '*_dictionary.rds' files that are more recent than 'data/dictionary.rda':")
  #     message(paste(paste0(" -- ", mod.files), collapse = "\n"))
  #     message("Would you like to recompile 'data/dictionary.rda'? (recommended)")
  #     message("1: Yes, please!\n2: Nah, I'm good.")
  #     answer <- readline("Answer: ")
  #     if (answer == 1) {
  #       #message("Re-compiling 'data/dictionary.rda'...")
  #       #suppressMessages(fusionData::compileDictionary())
  #       #message("Done.")
  #       fusionData::compileDictionary()
  #     }
  #   }
  # }
  #
  # #---
  #
  # # Identify package-critical files in /fusionData that show evidence of modification since package was last built
  # if (interactive()) {
  #
  #   mod.files <- pkg.files[tstamp > pkg.date]
  #
  #   # Ask user if they would like to re-install fusionData package
  #   if (length(mod.files) > 0) {
  #     message("Detected fusionData files modified since the package was last built:")
  #     message(paste(paste0(" -- ", mod.files), collapse = "\n"))
  #     message("Would you like to re-build the fusionData package? (recommended)")
  #     message("1: Yes, please!\n2: Nah, I'm good. ")
  #     answer <- readline("Answer: ")
  #     if (answer == 1) {
  #       #message("Re-installing fusionData package...")
  #       suppressMessages({
  #         devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))  # Update .rmd documentation files
  #         devtools::install(quick = TRUE, quiet = FALSE, dependencies = FALSE, upgrade = FALSE)  # Quick package install
  #         #system(paste("R CMD INSTALL", getwd(), "\ --no-docs --no-multiarch --no-demo"))
  #       })
  #       #message("Done.")
  #     }
  #   }
  # }

  #---

  # Print package information to console
  packageStartupMessage("fusionData v", utils::packageVersion("fusionData"), " | https://github.com/ummel/fusionData")

}
