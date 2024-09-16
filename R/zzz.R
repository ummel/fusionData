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
#' @import devtools
#' @import styler
#' @import tidyr
#' @rawNamespace import(shinyjs, except = c(html, runExample, alert))
#' @rawNamespace import(matrixStats, except = c(count))
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable, validate))
#' @rawNamespace import(matrixStats, except = c(count))
#' @rawNamespace import(stats, except = c(filter, lag))
#' @rawNamespace import(data.table, except = c(first, last, between, transpose))
NULL

#-----

.onLoad <- function (libname, pkgname) {

  # Prevent styler cache message; See ?styler::caching
  options(styler.cache_root = "styler")

  # Create default option value for number of cores
  options(fusionData.cores = max(1L, parallel::detectCores() - 1L))

  # Check that the current working directory is /fusionData
  if (basename(getwd()) != "fusionData") stop("The fusionData package requires the working directory to be /fusionData")

  # Deactivating for now -- oauth issues
  # If in interactive mode, ask user to authenticate in browser
  #if (interactive()) googledrive::drive_auth(email = "fusionacsdata@gmail.com")

  # Print package information to console
  packageStartupMessage("fusionData v", utils::packageVersion("fusionData"), " | https://github.com/ummel/fusionData")

  # Reminder about pulling latest updates from Github
  packageStartupMessage("Reminder: You might want to 'git pull' and installPackage() before using fusionData.")

}
