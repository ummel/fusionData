#' Open the fusionACS Survey Harmonization Tool
#'
#' @description
#' Opens the 'harmony' Shiny app (/harmony); i.e. fusionACS Survey Harmonization Tool.
#'
#' @return Opens the app in a new browser window. R session is occupied while that window is open.
#'
#' @examples
#' harmony()
#'
#' @export

harmony <- function() {
  shiny::runApp(appDir = "harmony",
                launch.browser = TRUE,
                quiet = TRUE,
                display.mode = "normal",
                test.mode = FALSE)
}
