.onLoad <- function(libname, pkgname) {
  # Prevent styler cache message; See ?styler::caching
  options(styler.cache_root = "styler")

  # Create default option value for number of cores
  options(fusionData.cores = max(1L, parallel::detectCores() - 1L))

  # Check that the current working directory is /fusionData
  if (basename(getwd()) != "fusionData") {
    stop("The fusionData package requires the working directory to be /fusionData")
  }
}

.onAttach <- function(libname, pkgname) {
  # Print package information to console
  packageStartupMessage("fusionData v", utils::packageVersion("fusionData"), " | https://github.com/ummel/fusionData")

  # Reminder about pulling latest updates from Github
  packageStartupMessage("Reminder: You might want to 'git pull' and installPackage() before using fusionData.")
}
