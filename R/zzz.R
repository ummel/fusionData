.onLoad <- function(libname, pkgname) {

  # Prevent styler cache message; See ?styler::caching
  options(styler.cache_root = "styler")

  # Create default option value for number of cores
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))  # Number of cores in Yale HPC environment
  if (is.na(ncores)) ncores <- max(1L, parallel::detectCores() - 1L)  # Generic fallback for local computing
  options(fusionData.cores = ncores)

}

.onAttach <- function(libname, pkgname) {

  # 1. Package Version and SHA Update Check
  current_ver <- utils::packageVersion("fusionData")

  # Always print package info first
  packageStartupMessage("fusionData v", current_ver, " | https://github.com/ummel/fusionData")

  # Extract local installed RemoteSha added by devtools/remotes
  local_sha <- utils::packageDescription("fusionData")$RemoteSha

  # Direct raw Atom feed check (bypasses GitHub API rate limits)
  remote_sha <- tryCatch({
    api_url <- "https://api.github.com/repos/ummel/fusionData/commits/master"

    opts <- options(timeout = 2)
    on.exit(options(opts), add = TRUE)

    # Explicit User-Agent header prevents GitHub network blocks
    con <- url(api_url, headers = c("User-Agent" = "fusionData-R-pkg"))
    on.exit(close(con), add = TRUE, TRUE)

    lines <- readLines(con, warn = FALSE)
    sha_line <- grep('"sha":', lines, value = TRUE)[1]

    if (!is.na(sha_line)) {
      sub('.*"sha":\\s*"([a-f0-9]{40})".*', '\\1', sha_line)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Alert based on remote SHA comparison
  if (!is.null(local_sha) && !is.null(remote_sha)) {
    if (local_sha != remote_sha) {
      packageStartupMessage("A newer commit (", substr(remote_sha, 1, 7), ") is available on GitHub. Run devtools::install_github('ummel/fusionData') to upgrade.")
    } else {
      packageStartupMessage("You are using the latest commit.")
    }
  } else if (is.null(local_sha)) {
    packageStartupMessage("Installed locally or via source without git remote tracking.")
  }

  # 2. Yale HPC Automated Working Directory Configuration
  yale_target_dir <- "/gpfs/milgram/project/rao/shared/fusionACS/fusionData"

  # Check if running within Yale HPC environment and target path exists
  is_yale_hpc <- dir.exists("/gpfs/milgram") & Sys.getenv("SLURM_CLUSTER_NAME") == "milgram"

  if (is_yale_hpc && dir.exists(yale_target_dir)) {
    setwd(yale_target_dir)
    packageStartupMessage("Yale HPC Milgram detected. Working directory set to:\n", yale_target_dir)
  }

  # Warn if directory is still not /fusionData after attempting HPC adjustment
  if (basename(getwd()) != "fusionData") {
    packageStartupMessage("Warning: The fusionData package requires the working directory to be /fusionData")
  }

}
