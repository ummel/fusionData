#' Install fusionData package locally
#'
#' @description
#' Fast and safe way to (re-)install the local version of the \code{fusionData}
#' package directly from source, mirroring RStudio's 'Install Package' build action.
#' Compiles internal survey dictionary files (\code{\link{compileDictionary}}),
#' updates package documentation and namespace via \code{\link[roxygen2]{roxygenise}},
#' and reinstalls the package.
#'
#' @details
#' When underlying survey microdata or spatial reference datasets are updated,
#' running this function ensures that regenerated \code{.rda} files in \code{data/}
#' are properly compiled, documented, and made accessible throughout the package.
#'
#' This function unloads the active \code{fusionData} namespace prior to installation
#' to prevent file-locking errors, and passes \code{build = FALSE} to reinstall
#' directly from local source files in seconds without archiving an intermediate
#' \code{.tar.gz} bundle.
#'
#' @examples
#' \dontrun{
#' installPackage()
#' }
#'
#' @export

installPackage <- function() {

  # Verify working directory is the package root
  if (basename(getwd()) != "fusionData") {
    cli::cli_abort("The working directory must be the {.path /fusionData} repository root.")
  }

  cli::cli_inform(c("i" = "Reminder: Run {.code git pull} first to ensure your local repository is up to date."))

  # Compile updated dictionary data files (.rda) into /data
  cli::cli_alert_info("Compiling dictionary files...")
  compileDictionary()

  # Update roxygen documentation and NAMESPACE
  cli::cli_alert_info("Updating package documentation and namespace...")
  suppressMessages({
    devtools::document(roclets = c("rd", "collate", "namespace", "vignette"))
  })

  # Unload active namespace to mirror RStudio's pre-install unload behavior
  cli::cli_alert_info("Unloading active namespace and rebuilding local package...")
  if ("fusionData" %in% loadedNamespaces()) {
    pkgload::unload("fusionData")
  }

  # Fast local source install (equivalent to RStudio's "Install Package" button)
  devtools::install(
    pkg = ".",
    reload = TRUE,
    quick = TRUE,
    build = FALSE,
    dependencies = TRUE,
    upgrade = FALSE,
  )

  cli::cli_alert_success("{.pkg fusionData} local package installation complete!")
}
