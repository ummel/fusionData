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
  current_ver <- utils::packageVersion("fusionData")

  # Always print package info first
  packageStartupMessage("fusionData v", current_ver, " | https://github.com/ummel/fusionData")

  # Direct raw URL check (bypasses GitHub API rate limits)
  remote_ver <- tryCatch({
    raw_url <- "https://raw.githubusercontent.com/ummel/fusionData/master/DESCRIPTION"

    # Set a strict 2-second timeout to prevent slowing down package load on slow connections
    opts <- options(timeout = 2)
    on.exit(options(opts), add = TRUE)

    desc_lines <- readLines(raw_url, warn = FALSE)
    ver_line <- grep("^Version:", desc_lines, value = TRUE)

    if (length(ver_line) > 0) {
      package_version(trimws(sub("^Version:", "", ver_line[1])))
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Alert based on remote version check
  if (!is.null(remote_ver)) {
    if (remote_ver > current_ver) {
      packageStartupMessage("A newer version (v", remote_ver, ") is available. Run devtools::install_github('ummel/fusionData') to upgrade.")
    } else {
      packageStartupMessage("You are using the latest version.")
    }
  }
}
