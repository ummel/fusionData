.onLoad <- function (libname, pkgname) {

  # Check that the working directory is /fusionData
  if (basename(getwd()) != "fusionData") warning("The fusionData package requires the working directory to be /fusionData")

  # Assign "mc.cores" object specifying number of cores to use
  assign("mc.cores", value = max(1L, parallel::detectCores() - 1L), envir = topenv())

  # Authenticate 'fusionACSdata' GDrive access using locally-stored token
  load("data/token.rda")
  googledrive::drive_auth(token = token)

  # NOTE: To generate/update the token, run this once in console
  # googledrive::drive_auth(email = "fusionacsdata@gmail.com")
  # token <- googledrive::drive_token()
  # save(token, file = "data/token.rda")

}
