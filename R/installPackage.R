#' Install fusionData package locally
#'
#' @description
#' Safe and convenient way to (re-)install the fusionData package locally. Compiles local survey dictionary files (\code{\link{compileDictionary}}); updates package documentation and namespace (\code{\link[roxygen2]{roxygenise}}); and then (re-)installs the package locally (\code{\link[devtools]{install}}). Missing package dependencies are installed by default; user is asked in console if they wish to upgrade already-installed dependencies.
#'
#' @examples
#' installPackage()
#'
#' @export

installPackage <- function() {

  # Check that the current working directory is /fusionData
  if (basename(getwd()) != "fusionData") stop("The working directory must be /fusionData")

  cat("Reminder: You should 'git pull' first to ensure you have the latest repository files...\n")

  # Rebuild local dictionary.rda and surveys.rda files
  cat("Compiling dictionary files...\n")
  fusionData::compileDictionary()

  cat("Updating package documentation and namespace...\n")
  suppressPackageStartupMessages({
    roxygen2::roxygenise()
  })

  cat("Building local fusionData package...\n")
  suppressPackageStartupMessages({
    devtools::install(quick = TRUE, dependencies = TRUE, upgrade = "ask")
  })

  cat("fusionData local package installation complete!\n")

}

