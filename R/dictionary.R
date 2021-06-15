#' Open the fusionACS Universal Survey Dictionary
#'
#' @description
#' Opens the 'dictionary' Shiny app (/dictionary); i.e. fusionACS Universal Survey Dictionary.
#'
#' @return Opens the app in a new browser window. R session is occupied while that window is open.
#'
#' @examples
#' dictionary()
#'
#' @export

dictionary <- function() {
  runApp(appDir = "dictionary",
         launch.browser = TRUE,
         quiet = TRUE,
         display.mode = "normal",
         test.mode = FALSE)
}
