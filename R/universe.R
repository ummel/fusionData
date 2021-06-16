#' Open the fusionACS Universal Survey Dictionary
#'
#' @description
#' Opens the 'universe' Shiny app (/universe); i.e. fusionACS Universal Survey Dictionary.
#'
#' @return Opens the app in a new browser window. R session is occupied while that window is open.
#'
#' @examples
#' universe()
#'
#' @export

universe <- function() {
  runApp(appDir = "universe",
         launch.browser = TRUE,
         quiet = TRUE,
         display.mode = "normal",
         test.mode = FALSE)
}
