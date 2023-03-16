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
#' @rawNamespace import(shinyjs, except = c(html, runExample, alert))
#' @rawNamespace import(matrixStats, except = c(count))
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable, validate))
#' @rawNamespace import(matrixStats, except = c(count))
#' @rawNamespace import(stats, except = c(filter, lag))
#' @rawNamespace import(data.table, except = c(first, last, between, transpose))
NULL

#-----

.onLoad <- function (libname, pkgname) {

  # Check that the current working directory is /fusionData
  if (basename(getwd()) != "fusionData") stop("The fusionData package requires the working directory to be /fusionData")

  # Create default option value for number of cores
  options(fusionData.cores = max(1L, parallel::detectCores() - 1L))

  # If in interactive mode, ask user to authenticate in browser
  if (interactive()) googledrive::drive_auth(email = "fusionacsdata@gmail.com")

  # Authenticate 'fusionACSdata' GDrive access using locally-stored token
  # load("data/token.rda")
  # googledrive::drive_deauth()
  # googledrive::drive_auth(token = token)

  # NOTE: To generate/update the token, run this once in console
  # googledrive::drive_auth(email = "fusionacsdata@gmail.com")
  # token <- googledrive::drive_token()
  # save(token, file = "data/token.rda")

  # Print package information to console
  packageStartupMessage("fusionData v", utils::packageVersion("fusionData"), " | https://github.com/ummel/fusionData")

}
