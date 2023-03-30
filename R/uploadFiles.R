#' Upload local fusionData files to remote Google Drive storage
#'
#' @description
#' Convenient and safe wrapper around \code{\link[googledrive]{googledrive}} functions to upload local file(s) in /fusionData to analogous location in /fusionData directory of "fusionACSdata" Google Drive account.
#'
#' @param files Character. Local file path(s) to be uploaded. Must be within local /fusionData directory.
#' @param ask Logical. Should user be prompted to confirm upload decision in console? Automatically set to \code{FALSE} if \code{interactive() == FALSE}.
#'
#' @return Local \code{files} uploaded to Google Drive account. Messages printed to console confirming actions.
#'
#' @export

#-----

uploadFiles <- function(files, ask = TRUE) {

  files <- normalizePath(path.expand(files))
  files <- unique(files)
  stopifnot({
    basename(getwd()) == "fusionData"
    all(file.exists(files))
    all(grepl("fusionData/", files, fixed = TRUE))
    is.logical(ask)
  })
  if (!interactive()) ask <- FALSE
  stubs <- sapply(strsplit(files, "fusionData/", fixed = TRUE), function(x) x[2])

  cat("Preparing to upload following file(s) to /fusionData directory in 'fusionACSdata' Google Drive:\n")
  cat(paste(stubs, collapse = "\n"), "\n\n")
  cat("The required directory hierarchy will be created, if necessary.\n")
  cat("Upload will overwrite existing remote file(s) if they exist.\n\n")
  ok <- if (ask) readline("Do you want to proceed with upload? (Y/N) ") else "Y"

  if (toupper(substring(ok, 1, 1)) == "Y") {

    # Authorize Google Drive
    googledrive::drive_auth(email = "fusionacsdata@gmail.com")

    # Check remote directory structure and create new folders, if necessary
    dirs <- file.path("~/fusionData", dirname(stubs))
    for (d in unique(dirs)) drive_dircreate(d)

    # Upload 'files' to remote storage
    for (i in seq_along(files)) {
      googledrive::drive_put(media = files[i],  # Local path
                             path = dirs[i],  # Remote directory
                             name = basename(files[i]))  # Remote file name
    }
    cat("uploadFiles(): File(s) successfully uploaded.\n")
  } else {
    cat("uploadFiles(): Upload canceled; file(s) were NOT uploaded.\n")
  }

}

#-----

# Function to create directory structure in Google Drive
drive_dircreate <- function(dir) {
  googledrive::drive_auth(email = "fusionacsdata@gmail.com")
  x <- strsplit(dir, split = "/", fixed = TRUE)[[1]]
  if (!x[1] %in% c("~", "My Drive")) stop("'dir' must start with 'My Drive' or '~' to denote home directory")
  x[1] <- "~"
  check <- TRUE
  cat("drive_dircreate(): Creating Google Drive directory structure for:\n", dir, "\n")
  for (i in 2:length(x)) {
    root <- paste(x[1:(i-1)], collapse = "/")
    if (check) {
      d <- googledrive::drive_ls(path = root, type = "folder")
      m <- sum(d$name == x[i])  # How many sub-directories match x[i]?
      if (m > 1) stop("Multiple existing directories for: ", file.path(root, x[i]))
      check <- m == 1
    }
    if (!check) googledrive::drive_mkdir(name = x[i], path = root)
  }
  cat("drive_dircreate() success:", dir, "\n")
}
