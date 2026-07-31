#' Open the fusionACS Universal Survey Dictionary
#'
#' @description
#' Launches the interactive 'universe' Shiny application to explore the master
#' survey dictionary and metadata compiled for the fusionACS project.
#'
#' @details
#' The app provides a searchable interface containing:
#' \itemize{
#'   \item \strong{Surveys}: Summary metadata detailing supported microdata
#'   surveys (e.g., ACS, AHS, CEI, NHTS, RECS, ASEC), survey vintages, respondent
#'   levels (Household vs. Person), sample sizes, variable counts, and file sizes.
#'   \item \strong{Variables}: A searchable dictionary of harmonized
#'   variable codes, descriptions, value coding, and survey linkages.
#' }
#'
#' Launching this function blocks the active R console session while the application
#' is running. To close the app and return to the R prompt, either close the
#' browser window or terminate the session in RStudio/console (e.g., press \code{Esc}
#' or click the stop icon).
#'
#' @return Opens the application in a new default web browser window. Returns
#' \code{NULL} invisibly upon closing.
#'
#' @examples
#' \dontrun{
#' # Open the survey dictionary app
#' universe()
#' }
#'
#' @export

universe <- function() {
  shiny::runApp(appDir = "universe",
                launch.browser = TRUE,
                quiet = TRUE,
                display.mode = "normal",
                test.mode = FALSE)
}
