.onLoad <- function (libname, pkgname) {

  # Check that the working directory is /fusionData
  if (basename(getwd()) != "fusionData") warning("The fusionData package requires the working directory to be /fusionData")

  # Assign "mc.cores" object specifying number of cores to use
  assign("mc.cores", value = max(1L, parallel::detectCores() - 1L), envir = topenv())

}
