#' Open the fusionACS Survey Harmonization Tool
#'
#' @description
#' Launches the interactive \bold{Survey Harmonization Tool} Shiny application.
#' This app provides a GUI to map, group, and align variable categories between
#' donor surveys (e.g., RECS, CEX, NHIS) and the recipient American Community
#' Survey (ACS) microdata.
#'
#' @details
#' The harmonization step is critical to the \code{fusionACS} workflow. It allows
#' users to map factor levels or continuous concepts between raw donor variables
#' and ACS target variables, producing standardized harmony specifications stored
#' as \code{.R} files in \code{/harmony/harmonies}.
#'
#' Running this function blocks the active R console session while the local web
#' application is active. To return to the R prompt, close the browser window or
#' press \kbd{Esc} / \kbd{Ctrl+C} in the R console.
#'
#' @return Launches the Shiny application in the default web browser. Returns
#'   \code{NULL} invisibly when the app is closed.
#'
#' @examples
#' \dontrun{
#' # Launch the interactive harmonization GUI
#' harmony()
#' }
#'
#' @export

harmony <- function() {

  # Locate the harmony app directory within the installed package or local workspace
  app_dir <- system.file("harmony", package = "fusionData")
  if (app_dir == "") {
    app_dir <- "harmony"
  }

  # Inform user in console using cli formatting
  cli::cli_alert_info("Launching the fusionACS Survey Harmonization Tool...")
  cli::cli_alert_info("Press {.kbd Esc} or close the browser window to stop the application.")

  # Launch Shiny application
  shiny::runApp(appDir = app_dir,
                launch.browser = TRUE,
                quiet = TRUE,
                display.mode = "normal",
                test.mode = FALSE)
}
