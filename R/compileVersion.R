#' Compile fusionACS Database Version and Optional Public Release
#'
#' @description
#' Compiles a full-resolution, multi-implicate version of the \code{fusionACS} microdata
#' database on the Yale HPC facility (Milgram cluster) from processed ACS microdata,
#' donor survey implicates, and UrbanPop synthetic population files. Optionally
#' generates and uploads a single-implicate pseudo-sample release to the
#' fusionACS package GitHub repository using \code{piggyback}.
#' The release is then accessible to the public via [fusionACS::get_microdata()].
#'
#' @details
#' Execution is strictly restricted to the Yale HPC Milgram cluster and requires
#' the working directory to be set to
#' \code{/gpfs/milgram/project/rao/shared/fusionACS/fusionData}. Parallel thread
#' allocation for Apache Arrow operations is automatically configured based on
#' SLURM CPU task allocation (\code{SLURM_CPUS_PER_TASK}).
#'
#' The function executes in two distinct phases:
#' \enumerate{
#'   \item \strong{Full Database Compilation (\code{!dir.exists(target_dir)}):}
#'         Aggregates geographic concordance tables, spatial block group crosswalks,
#'         UrbanPop synthetic population location lookups, ACS household and person
#'         microdata, and donor survey implicates into structured Apache Arrow Parquet
#'         datasets at \code{fusionACS/versions/<version_date>}. To optimize performance
#'         and storage, unchanged inputs relative to the most recent prior version
#'         are automatically detected and linked via filesystem symbolic links.
#'         If an unhandled error occurs mid-compilation, an \code{on.exit} hook automatically
#'         removes the incomplete target directory to prevent corruption.
#'   \item \strong{Public Release Generation (\code{public_release = TRUE}):}
#'         Constructs a public-facing pseudo-sample in \code{fusionACS/public_releases/<version_date>}. A single
#'         implicate (\code{M = 1}) is isolated for donor surveys, and UrbanPop households
#'         are assigned to 2010/2020 Census Block Groups via a weighted exponential
#'         random sampling algorithm. Output components are linked, bundled into
#'         three partitioned uncompressed \code{.tar} archives (dereferencing symlinks
#'         via \code{tar -h}), and uploaded as GitHub release assets under tag
#'         \code{<version_date>}.
#' }
#'
#' If the compiled \code{target_dir} already exists on disk and \code{public_release = TRUE},
#' the function automatically skips database compilation and proceeds directly to public
#' release generation and upload. If an identical GitHub release tag already exists,
#' the user is prompted before overwriting.
#'
#' @param version_date Character. Version release identifier formatted as
#'   \code{"YYYY-MM-DD"}. Defaults to the current system date (\code{as.character(Sys.Date())}).
#' @param public_release Logical. If \code{TRUE} (default), generates and uploads
#'   the single-implicate public pseudo-sample dataset to the fusionACS package GitHub repository.
#'
#' @note
#' The public release phase actively queries the GitHub API with local cache clearing (\code{piggyback::.pb_cache_clear()})
#' to account for propagation delays during release deletion and creation.
#'
#' @return Invisibly returns the absolute path to the compiled version directory (\code{target_dir}).
#'
#' @seealso \code{\link[piggyback]{pb_upload}}
#' @export

compileVersion <- function(version_date = as.character(Sys.Date()),
                           public_release = TRUE) {

  # Helper functions for checking and querying Linux symbolic links via system calls.
  is_symlink <- function(path) {
    system2("test", c("-L", shQuote(path))) == 0
  }
  symlink_target <- function(path) {
    system2("readlink", c("-f", shQuote(path)), stdout = TRUE)
  }

  is_yale_hpc <- dir.exists("/gpfs/milgram") && Sys.getenv("SLURM_CLUSTER_NAME") == "milgram"
  if (!is_yale_hpc) {
    cli::cli_abort("`compileVersion()` must be executed on the Yale HPC (Milgram cluster).")
  }

  expected_wd <- "/gpfs/milgram/project/rao/shared/fusionACS/fusionData"
  if (normalizePath(getwd(), winslash = "/", mustWork = FALSE) != expected_wd) {
    cli::cli_abort("Working directory must be set to {.path {expected_wd}}. Current wd: {.path {getwd()}}")
  }

  cli::cli_alert_success("Yale HPC environment and working directory verified.")

  # Configure CPU thread count for Arrow dataset operations based on SLURM task allocation.
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
  if (is.na(ncores) || ncores < 1) ncores <- parallel::detectCores()
  arrow::set_cpu_count(ncores)
  cli::cli_alert_info("Using {ncores} CPU core(s) for execution.")

  # Base constants
  base_dir <- "/gpfs/milgram/project/rao/shared/fusionACS"
  versions_root <- "/gpfs/milgram/project/rao/shared/fusionACS/versions"
  target_dir <- file.path(versions_root, version_date)
  repo <- "ummel/fusionACS"

  # ---------------------------------------------------------------------------
  # 1. Directory Checks & Branching Logic
  # ---------------------------------------------------------------------------
  dir_exists <- dir.exists(target_dir)

  if (dir_exists) {
    if (public_release) {
      cli::cli_alert_info(
        "Target version directory {.path {target_dir}} already exists. Skipping database compilation and proceeding to public release generation."
      )
    } else {
      cli::cli_abort(
        "Target directory {.path {target_dir}} already exists. Delete it or specify a different {.arg version_date} before proceeding."
      )
    }
  }

  # Load GitHub Personal Access Token for 'ummel/fusionACS'
  token <- trimws(last(readLines("/gpfs/milgram/project/rao/shared/fusionACS/.github_token", warn = FALSE)))

  # Check if release tag already exists on GitHub
  if (public_release) {
    piggyback::.pb_cache_clear()
    delete_release <- FALSE
    if (version_date %in% piggyback::pb_releases(repo = repo, .token = token)$tag_name) {
      cli::cli_alert_warning("Release tag {.val {version_date}} already exists on GitHub ({repo}).")
      proceed <- utils::askYesNo("Do you want to overwrite the existing release on GitHub?")
      if (isTRUE(proceed)) {
        delete_release <- TRUE
      } else {
        cli::cli_abort("Public release generation aborted by user.")
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Complete Database Compilation Phase
  # ---------------------------------------------------------------------------
  if (!dir_exists) {

    cli::cli_h2("Compiling full fusionACS database version: {.val {version_date}}")
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_success("Created target output directory:\n{.path {target_dir}}")

    # Establish cleanup trigger: If compilation fails mid-way, delete corrupted target_dir
    on.exit({
      if (dir.exists(target_dir) && !file.exists(file.path(target_dir, ".complete"))) {
        cli::cli_alert_warning("Compilation incomplete or failed. Removing directory {.path {target_dir}}.")
        unlink(target_dir, recursive = TRUE)
      }
    }, add = TRUE)

    # Automate detection of the most recent prior version directory in /versions.
    existing_dirs <- list.dirs(versions_root, full.names = TRUE, recursive = FALSE)
    dir_dates <- basename(existing_dirs)
    valid_idx <- grepl("^\\d{4}-\\d{2}-\\d{2}$", dir_dates) & (dir_dates < version_date)

    if (any(valid_idx)) {
      prior_date <- max(dir_dates[valid_idx])
      prior.path <- file.path(versions_root, prior_date)
      cli::cli_alert_info("Identified most recent prior version: {.path {prior.path}}")
    } else {
      prior.path <- NULL
      cli::cli_alert_warning("No prior version directory found before {.val {version_date}}.")
    }

    # Prepare dependencies and prior target files for spatial data checking
    geo_con_files <- c(
      "geo-processed/concordance/geo_concordance_2010.fst",
      "geo-processed/concordance/geo_concordance_2020.fst"
    )
    geo_xwalk_file <- "data/bg_crosswalk.rda"

    flist19 <- list.files("urbanpop/v3/2015-2019", pattern = "00.fst$", recursive = TRUE, full.names = TRUE)
    flist23 <- list.files("urbanpop/v3/2019-2023", pattern = "00.fst$", recursive = TRUE, full.names = TRUE)

    prior_geography <- if (!is.null(prior.path)) file.path(prior.path, "geography.parquet") else ""
    prior_location  <- if (!is.null(prior.path)) file.path(prior.path, "location.parquet") else ""

    # ---------------------------------------------------------------------------
    # GEOGRAPHIC CONCORDANCE DATA & GEO-CROSSWALK CONSTRUCTION
    # ---------------------------------------------------------------------------
    cli::cli_h2("Processing Geographic Concordance & Crosswalk")

    cli::cli_alert_info("Compiling geography crosswalk...")

    # Load pre-processed 2010 and 2020 Census geographic concordance tables.
    geocon10 <- fst::read_fst("geo-processed/concordance/geo_concordance_2010.fst", as.data.table = TRUE) %>%
      dplyr::filter(bg10 != "None") %>%
      dplyr::mutate(bg10 = paste0(state, county10, tract10, bg10))

    geocon20 <- fst::read_fst("geo-processed/concordance/geo_concordance_2020.fst", as.data.table = TRUE) %>%
      dplyr::filter(bg20 != "None") %>%
      dplyr::mutate(bg20 = paste0(state, county20, tract20, bg20))

    stopifnot(all(nchar(geocon10$bg10) == 12))
    stopifnot(all(nchar(geocon20$bg20) == 12))

    # Aggregate PUMA population weights by block group for 2010 boundaries
    geoxwalk10 <- geocon10[, .(weight = sum(puma_weight)),
                           by = .(region, division, state, state_postal, state_name, puma10, county10, tract10, bg10, cousub10, ur10, zcta10, cbsa10, csa10)]
    geoxwalk10[, gfactor := weight / sum(weight), by = bg10]
    geoxwalk10 <- geoxwalk10 %>%
      dplyr::select(-weight) %>%
      dplyr::mutate(
        puma10 = paste0(state, puma10),
        cousub10 = paste0(state, county10, cousub10),
        tract10 = paste0(state, county10, tract10),
        county10 = paste0(state, county10)
      )

    # Aggregate PUMA population weights by block group for 2020 boundaries
    geoxwalk20 <- geocon20[, .(weight = sum(puma_weight)),
                           by = .(region, division, state, state_postal, state_name, puma20, county20, tract20, bg20, cousub20, ur20, zcta20, cbsa20, csa20)]
    geoxwalk20[, gfactor := weight / sum(weight), by = bg20]
    geoxwalk20 <- geoxwalk20 %>%
      dplyr::select(-weight) %>%
      dplyr::mutate(
        puma20 = paste0(state, puma20),
        cousub20 = paste0(state, county20, cousub20),
        tract20 = paste0(state, county20, tract20),
        county20 = paste0(state, county20)
      )

    # Load 2010-2020 block group crosswalk file (bg_crosswalk.rda)
    load(file.path(base_dir, "fusionData/data/bg_crosswalk.rda"))
    static <- c("region", "division", "state", "state_postal", "state_name")

    # Merge 2010 and 2020 geographic levels with the block group crosswalk.
    geoxwalk <- bg_crosswalk %>%
      merge(geoxwalk20, by = "bg20", all = TRUE, allow.cartesian = TRUE) %>%
      merge(geoxwalk10, by = c("bg10", static), suffixes = c("20", "10"), all = TRUE, allow.cartesian = TRUE) %>%
      dplyr::mutate(weight = as.integer(round(xwalk_weight * gfactor10 * gfactor20))) %>%
      dplyr::filter(weight > 0) %>%
      dplyr::select(-xwalk_weight, -dplyr::starts_with("gfactor")) %>%
      dplyr::select(weight, dplyr::all_of(static), dplyr::ends_with('20'), dplyr::ends_with('10')) %>%
      stats::na.omit()

    v <- collapse::cat_vars(geoxwalk, return = "names")
    geoxwalk[, (v) := lapply(.SD, as.factor), .SDcols = v]
    data.table::setorder(geoxwalk, bg20, bg10, weight)

    stopifnot(!any(substring(geoxwalk$bg20, 1, 2) != substring(geoxwalk$bg10, 1, 2), na.rm = TRUE))

    # Check MD5 hash of compiled geoxwalk against prior_geography
    use_geography_symlink <- FALSE
    if (!is.null(prior.path) && file.exists(prior_geography)) {
      tmp_geo <- tempfile(fileext = ".parquet")
      arrow::write_parquet(geoxwalk, tmp_geo, compression = "snappy")
      new_md5 <- unname(tools::md5sum(tmp_geo))
      prior_md5 <- unname(tools::md5sum(prior_geography))
      unlink(tmp_geo)
      if (identical(new_md5, prior_md5)) use_geography_symlink <- TRUE
    }

    geo_target_path <- file.path(target_dir, "geography.parquet")

    if (use_geography_symlink) {
      cli::cli_alert_success("Geography crosswalk matches prior version (MD5 checksum); creating symlink.")
      file.symlink(from = prior_geography, to = geo_target_path)
    } else {
      arrow::write_parquet(geoxwalk, sink = geo_target_path, compression = "snappy")
      cli::cli_alert_success("Saved updated geography crosswalk to:\n{.path {geo_target_path}}")
    }

    # ---------------------------------------------------------------------------
    # URBANPOP DATA PROCESSING (LOCATION TABLE)
    # ---------------------------------------------------------------------------
    cli::cli_h2("Processing UrbanPop Data")

    location_inputs <- c(flist19, flist23)
    use_location_symlink <- FALSE

    if (!is.null(prior.path) && file.exists(prior_location) && all(file.exists(location_inputs)) && length(location_inputs) > 0) {
      input_mtimes <- as.Date(file.info(location_inputs)$mtime)
      if (use_geography_symlink && all(input_mtimes <= as.Date(prior_date))) {
        use_location_symlink <- TRUE
      }
    }

    if (use_location_symlink) {
      cli::cli_alert_success("UrbanPop and geographic concordance inputs unchanged since prior_date ({prior_date}); creating symlink.")
      file.symlink(from = prior_location, to = file.path(target_dir, "location.parquet"))
    } else {
      cli::cli_alert_info("Compiling UrbanPop location data...")

      # Load concordance block group vectors for filtering
      geocon10_bgs <- fst::read_fst("geo-processed/concordance/geo_concordance_2010.fst", columns = c("state", "county10", "tract10", "bg10"), as.data.table = TRUE) %>%
        dplyr::filter(bg10 != "None") %>%
        .[, bg10 := paste0(state, county10, tract10, bg10)] %>%
        dplyr::pull(bg10) %>%
        unique()

      geocon20_bgs <- fst::read_fst("geo-processed/concordance/geo_concordance_2020.fst", columns = c("state", "county20", "tract20", "bg20"), as.data.table = TRUE) %>%
        dplyr::filter(bg20 != "None") %>%
        .[, bg20 := paste0(state, county20, tract20, bg20)] %>%
        dplyr::pull(bg20) %>%
        unique()

      # Read synthetic population household location files for 2015–2019
      upop19 <- flist19 %>%
        lapply(fst::read_fst, as.data.table = TRUE) %>%
        data.table::rbindlist(fill = TRUE)

      upop19[, bg10 := collapse::finteraction(
        stringr::str_pad(state, width = 2, pad = "0"),
        stringr::str_pad(county10, width = 3, pad = "0"),
        stringr::str_pad(tract10, width = 6, pad = "0"),
        bg10,
        sep = ""
      )]
      upop19 <- upop19[bg10 %iin% geocon10_bgs]

      # Read synthetic population household location files for 2019–2023
      upop23 <- flist23 %>%
        lapply(fst::read_fst, as.data.table = TRUE) %>%
        data.table::rbindlist(fill = TRUE)

      upop23[, bg20 := collapse::finteraction(
        stringr::str_pad(state, width = 2, pad = "0"),
        stringr::str_pad(county20, width = 3, pad = "0"),
        stringr::str_pad(tract20, width = 6, pad = "0"),
        bg20,
        sep = ""
      )]
      upop23 <- upop23[bg20 %iin% geocon20_bgs]

      # Combine into location lookup table
      upop <- rbind(upop19, upop23, fill = TRUE)
      rm(upop19, upop23)
      upop <- dplyr::select(upop, year, hid, bg10, bg20, weight)
      data.table::setorder(upop, bg10, bg20, year, hid)

      arrow::write_parquet(upop, sink = file.path(target_dir, "location.parquet"), compression = "snappy")
      cli::cli_alert_success("Saved location data to:\n{.path {file.path(target_dir, 'location.parquet')}}")
    }

    # ---------------------------------------------------------------------------
    # PROCESS ACS MICRODATA
    # ---------------------------------------------------------------------------
    cli::cli_h2("Processing ACS Microdata")

    fused_files <- list.files("fusion", pattern = "_[HP]_fused\\.fsd$", recursive = TRUE)
    if (length(fused_files) == 0) {
      cli::cli_abort("No fusion output files matching pattern `_[HP]_fused.fsd` found in `fusion/`.")
    }

    acs_years_used <- stringr::str_extract(fused_files, "\\d{4}(?=_[HP]_fused\\.fsd$)") %>%
      as.integer() %>%
      stats::na.omit() %>%
      unique()

    if (length(acs_years_used) == 0) {
      cli::cli_abort("Failed to parse ACS respondent years from fusion filenames in `fusion/`.")
    }

    min_acs_year <- min(acs_years_used)
    max_acs_year <- max(acs_years_used)
    cli::cli_alert_info("Inferred ACS respondent year range from fusion outputs: {min_acs_year} to {max_acs_year}")

    # Scan once for processed ACS files
    all_p_files <- list.files(
      path = "survey-processed/ACS",
      pattern = "_P_processed\\.fst$",
      recursive = TRUE,
      full.names = TRUE
    )

    # Match exact expected file names for the requested year range
    target_years <- min_acs_year:max_acs_year
    expected_filenames <- sprintf("ACS_%d_P_processed.fst", target_years)
    acs.flist <- all_p_files[basename(all_p_files) %in% expected_filenames]

    acs_input_files <- acs.flist
    acs_input_files <- c(acs_input_files, sub("_P_processed\\.fst$", "_H_processed.fst", acs.flist))
    acs_input_files <- c(acs_input_files, sub("_processed\\.fst$", "_custom.fst", acs_input_files))
    acs_input_files <- acs_input_files[file.exists(acs_input_files)]

    acs_prior_h <- if (!is.null(prior.path)) file.path(prior.path, "ACS_H.parquet") else ""
    acs_prior_p <- if (!is.null(prior.path)) file.path(prior.path, "ACS_P.parquet") else ""

    use_acs_symlink <- FALSE
    if (!is.null(prior.path) && file.exists(acs_prior_h) && file.exists(acs_prior_p)) {
      input_mtime_dates <- as.Date(file.info(acs_input_files)$mtime)
      prior_date_obj <- as.Date(prior_date)

      if (all(input_mtime_dates <= prior_date_obj)) {
        use_acs_symlink <- TRUE
      }
    }

    if (use_acs_symlink) {
      cli::cli_alert_success("ACS microdata files unchanged since prior_date ({prior_date}); creating symlinks.")
      file.symlink(from = acs_prior_h, to = file.path(target_dir, "ACS_H.parquet"))
      file.symlink(from = acs_prior_p, to = file.path(target_dir, "ACS_P.parquet"))
    } else {
      cli::cli_alert_info("Compiling ACS microdata for years {min_acs_year}-{max_acs_year}...")

      # Load geography header names to drop redundant geography columns
      geoxwalk_cols <- c("region", "division", "state", "state_postal", "state_name",
                         "puma20", "county20", "tract20", "bg20", "cousub20", "ur20", "zcta20", "cbsa20", "csa20",
                         "puma10", "county10", "tract10", "bg10", "cousub10", "ur10", "zcta10", "cbsa10", "csa10")

      get_acs_data <- function(x) {
        v <- fst::fst.metadata(x)$columnNames
        v <- grep("rep_\\d+$", v, value = TRUE, invert = TRUE)
        vpuma <- intersect(v, c('puma10', 'puma20'))

        p <- fst::read_fst(x, columns = v, as.data.table = TRUE) %>%
          dplyr::mutate(
            !!vpuma := factor(paste0(
              stringr::str_pad(state, width = 2, pad = "0"),
              stringr::str_pad(.data[[vpuma]], width = 5, pad = "0")
            ))
          ) %>%
          dplyr::select(-dplyr::any_of(setdiff(geoxwalk_cols, vpuma)))

        xc <- sub("_processed.fst", "_custom.fst", x)
        if (file.exists(xc)) p <- merge(p, fst::read_fst(xc, as.data.table = TRUE), by = c('year', 'hid', 'pid'))

        x_h <- sub("P_processed.fst$", "H_processed.fst", x)
        v_h <- fst::fst.metadata(x_h)$columnNames
        v_h <- grep("rep_\\d+$", v_h, value = TRUE, invert = TRUE)

        h <- fst::read_fst(x_h, columns = v_h, as.data.table = TRUE) %>%
          dplyr::mutate(
            !!vpuma := factor(paste0(
              stringr::str_pad(state, width = 2, pad = "0"),
              stringr::str_pad(.data[[vpuma]], width = 5, pad = "0")
            ))
          ) %>%
          dplyr::select(-dplyr::any_of(setdiff(geoxwalk_cols, vpuma)))

        xc_h <- sub("_processed.fst", "_custom.fst", x_h)
        if (file.exists(xc_h)) h <- merge(h, fst::read_fst(xc_h, as.data.table = TRUE), by = c('year', 'hid'))

        list(h, p)
      }

      # Warnings here are acceptable from rbindlist()
      suppressWarnings({
        acs <- acs.flist %>%
          lapply(get_acs_data) %>%
          purrr::transpose() %>%
          purrr::map(data.table::rbindlist, use.names = TRUE, fill = TRUE, ignore.attr = TRUE) %>%
          stats::setNames(c("H", "P"))
      })

      arrow::write_parquet(acs$H, sink = file.path(target_dir, "ACS_H.parquet"), compression = "snappy")
      arrow::write_parquet(acs$P, sink = file.path(target_dir, "ACS_P.parquet"), compression = "snappy")
      cli::cli_alert_success("Saved ACS microdata (ACS_H.parquet and ACS_P.parquet)")

      rm(acs)
      gc()
    }

    # ---------------------------------------------------------------------------
    # PROCESS DONOR SURVEY FUSION OUTPUTS
    # ---------------------------------------------------------------------------
    cli::cli_h2("Processing Donor Survey Data")

    finfo <- data.frame(fsd = list.files("fusion", pattern = "_fused\\.fsd$", recursive = TRUE)) %>%
      dplyr::mutate(
        donor = sub("/", "_", stringr::str_extract(fsd, "^[^/]+/[^/]+")),
        acs_year = stringr::str_sub(fsd, -16, -13),
        version = as.Date(regmatches(fsd, regexpr("\\d{4}-\\d{2}-\\d{2}", fsd))),
        fsd = file.path("fusion", fsd)
      ) %>%
      dplyr::group_by(donor, acs_year) %>%
      dplyr::slice_max(version) %>%
      dplyr::ungroup()

    if (!is.null(prior.path) && file.exists(file.path(prior.path, "dictionary.parquet"))) {
      prior.meta <- arrow::read_parquet(file.path(prior.path, "dictionary.parquet")) %>%
        dplyr::mutate(donor = paste(survey, vintage, sep = "_")) %>%
        dplyr::distinct(donor, version, file)
    } else {
      prior.meta <- data.frame(donor = character(), version = as.Date(character()), file = character())
    }

    for (dv in unique(finfo$donor)) {
      cli::cli_alert_info("Processing donor survey: {.val {dv}}")

      check <- finfo %>%
        dplyr::filter(donor == dv) %>%
        dplyr::left_join(prior.meta, by = c('donor', 'version'))

      old <- if (nrow(check) > 0 && !is.null(prior.path)) file.path(prior.path, unique(check$file)) else ""

      if (length(old) > 0 && all(nzchar(old)) && all(file.exists(old))) {
        cli::cli_alert_success(" -- Target version already on disk; creating symlink.")
        file.symlink(from = old, to = file.path(target_dir, unique(check$file)))
      } else {
        cli::cli_alert_info(" -- Compiling microdata...")

        flist_donor <- finfo %>%
          dplyr::filter(donor == dv) %>%
          dplyr::pull(fsd)
        flist_donor <- split(flist_donor, stringr::str_extract(flist_donor, "_[HP]_"))
        names(flist_donor) <- stringr::str_remove_all(names(flist_donor), "_")

        get_donor_data <- function(fpaths) {

          # Warnings here are acceptable from rbin
          suppressWarnings({
            d <- fpaths %>%
              lapply(fusionModel::read_fsd, M = 20, cores = ncores) %>%  # Only read first 20 implicates (in case there are more)
              data.table::rbindlist(use.names = TRUE, fill = TRUE, ignore.attr = TRUE)
          })

          if (length(d)) {
            if ('pid' %in% names(d)) {
              data.table::setorder(d, M, year, hid, pid)
            } else {
              data.table::setorder(d, M, year, hid)
            }
          }
          return(d)
        }

        h <- get_donor_data(flist_donor$H)
        if (length(h)) arrow::write_dataset(h, path = file.path(target_dir, paste0(dv, "_H")), partitioning = "M", compression = "snappy")
        rm(h)

        p <- get_donor_data(flist_donor$P)
        if (length(p)) arrow::write_dataset(p, path = file.path(target_dir, paste0(dv, "_P")), partitioning = "M", compression = "snappy")
        rm(p)

        gc()
      }
    }

    # ---------------------------------------------------------------------------
    # CONSTRUCT METADATA DICTIONARY
    # ---------------------------------------------------------------------------
    cli::cli_h2("Constructing Variable Metadata Dictionary")

    dfiles <- list.files(path = "survey-processed", pattern = "_._dictionary.rds", recursive = TRUE, full.names = TRUE)
    dictionary <- dfiles %>%
      purrr::map_dfr(readRDS) %>%
      dplyr::mutate(respondent = ifelse(substring(tolower(respondent), 1, 1) == "h", "Household", "Person")) %>%
      dplyr::bind_rows(readRDS("geo-processed/concordance/geo_concordance_definitions_2010.rds") %>%
                         dplyr::mutate(survey = "geography")) %>%
      dplyr::bind_rows(readRDS("geo-processed/concordance/geo_concordance_definitions_2020.rds") %>%
                         dplyr::mutate(survey = "geography"))

    uvar <- c('year', 'hid', 'pid', 'weight')

    create_metadata <- function(f) {
      if (is_symlink(f)) {
        out <- arrow::read_parquet(file = file.path(dirname(symlink_target(f)), "dictionary.parquet")) %>%
          dplyr::filter(file == basename(f))
      } else {
        svy <- dplyr::case_when(
          startsWith(basename(f), "ACS_") ~ "ACS",
          startsWith(basename(f), "geography") ~ "geography",
          .default = stringr::str_sub(basename(f), end = -3)
        )

        res <- dplyr::case_when(
          stringr::str_sub(sub(".parquet$", "", basename(f)), -1) == "H" ~ "household",
          stringr::str_sub(sub(".parquet$", "", basename(f)), -1) == "P" ~ "person",
          .default = NA
        )

        ver <- dplyr::filter(finfo, donor == svy)$version[1]

        d <- arrow::open_dataset(f)
        if ("M" %in% names(d)) d <- dplyr::filter(d, M == 1)
        d <- collect(d)

        out <- lapply(setdiff(names(d), uvar), function(v) {
          x <- d[[v]]
          if (is.character(x)) x <- factor(x)

          y <- collapse::fsum(is.na(x), g = d$year)
          y <- list(as.integer(names(y)[y == 0]))
          if (length(unlist(y)) == 0) y <- NA_integer_

          out_var <- dictionary %>%
            dplyr::filter(
              variable == v,
              survey == stringr::str_split_i(svy, "_", 1),
              vintage %in% if (svy == "ACS") unlist(y) else stringr::str_split_i(svy, "_", 2),
              respondent %in% stringr::str_to_title(res)
            ) %>%
            dplyr::slice_max(nchar(description), n = 1, with_ties = FALSE) %>%
            dplyr::mutate(
              vintage = ifelse(survey == "ACS", NA_integer_, vintage),
              respondent = tolower(respondent),
              n_values = data.table::uniqueN(x, na.rm = TRUE),
              years = y,
              version = ver,
              file = basename(f)
            )

          out_var$values <- if (is.numeric(x)) {
            paste(
              c("Min:", "1st Quartile:", "Median:", "Mean:", "3rd Quartile:", "Max:"),
              summary(x, digits = 4),
              collapse = ", "
            )
          } else {
            if (is.logical(x)) {
              list(c('TRUE', 'FALSE'))
            } else {
              z <- levels(x)
              if (length(z) > 100) z <- c(z[1:100], "[TRUNCATED]")
              list(z)
            }
          }

          out_var$type <- switch(
            vctrs::vec_ptype_abbr(x),
            fct = 'categorical (unordered)',
            ord = 'categorical (ordered)',
            int = 'integer',
            dbl = 'double',
            lgl = 'logical'
          )

          return(out_var)
        }) %>%
          data.table::rbindlist() %>%
          dplyr::select(variable, description, dplyr::everything())
      }

      return(out)
    }

    flist_meta <- list.files(target_dir, pattern = "[HP].parquet$", full.names = TRUE)
    flist_meta <- c(flist_meta, list.dirs(target_dir, full.names = TRUE, recursive = FALSE))
    flist_meta <- c(flist_meta, file.path(target_dir, "geography.parquet"))

    meta <- flist_meta %>%
      lapply(create_metadata) %>%
      data.table::rbindlist(fill = TRUE, ignore.attr = TRUE) %>%
      dplyr::arrange(survey, respondent, variable, vintage) %>%
      dplyr::select(variable, description, survey, vintage, respondent, type, n_values, values, years, custom, version, file)

    arrow::write_parquet(meta, sink = file.path(target_dir, "dictionary.parquet"), compression = "snappy")
    cli::cli_alert_success("Metadata dictionary saved to:/n{.path {file.path(target_dir, 'dictionary.parquet')}}")

    # Mark compilation as successfully completed
    file.create(file.path(target_dir, ".complete"))
    cli::cli_alert_success("Full database version compiled at:\n{.path {target_dir}}")

    # Set the local fusionACS package data directory to the latest version
    fusionACS::set_directory(target_dir)
  }

  # Exit early if public release is not requested
  if (!public_release) {
    return(invisible(target_dir))
  }

  # ---------------------------------------------------------------------------
  # 3. Public Release Generation & GitHub Upload Phase
  # ---------------------------------------------------------------------------
  cli::cli_h2("Generating public Github release for version {.val {version_date}}")

  # Define and create public releases working directory
  release_dir <- file.path(base_dir, "public_releases", version_date)
  unlink(release_dir, recursive = TRUE)  # Ensures it is replaced entirely if it already exists
  dir.create(release_dir, recursive = TRUE)

  # --- A. Create geography.parquet ---
  # PURPOSE: Build a clean, 1-to-1 mapping for 2010 and 2020 Census Block Groups (bg10/bg20).
  # WHY: Some block groups span multiple secondary geographies (e.g., ZIP code boundaries).
  # HOW: By ordering by `weight` descending and selecting `.SD[1L]` per block group pair,
  # we isolate the single most spatial/population-dominant geographic mapping. Setting
  # weight := 1L normalizes this crosswalk entry for downstream joining.
  cli::cli_alert_info("Generating public geographic crosswalk ({.file geography.parquet})...")
  geoxwalk <- arrow::read_parquet(file.path(target_dir, "geography.parquet"))
  data.table::setDT(geoxwalk)
  geoxwalk <- geoxwalk[order(-weight), .SD[1L], by = c('bg10', 'bg20')]
  geoxwalk[, weight := 1L]
  arrow::write_parquet(geoxwalk, sink = file.path(release_dir, "geography.parquet"), compression = "snappy")

  # --- B. Create UrbanPop pseudo-sample location.parquet ---
  # PURPOSE: Construct a spatial pseudo-sample location table for UrbanPop households.
  # WHY: Public releases require realistic spatial allocation without exposing full microdata
  # dependencies. We perform a probability-weighted iterative selection algorithm to sample
  # households into block groups matching target ACS/UrbanPop population distributions.
  cli::cli_alert_info("Generating UrbanPop spatial pseudo-sample ({.file location.parquet})...")
  d <- arrow::read_parquet(file.path(target_dir, "location.parquet"))
  data.table::setDT(d)

  # Keep only columns necessary for spatial sampling to reduce RAM footprint
  d <- d[, .(year, hid, bg10, bg20, weight)]
  d[, version := data.table::fifelse(is.na(bg20), 1L, 2L)] # This is necessary, because there is overlap for year 2019 in the two UrbanPop vintages: 1 = 2015-2019; 2 = 2019-2023

  # Restrict to block groups present in geography crosswalk
  d <- d[bg10 %in% geoxwalk$bg10 | bg20 %in% geoxwalk$bg20]

  # Compute group sizes and weighted sampling probability
  # WHY: Converting 'weight' via -log(runif(.N)) / weight creates Gumbel-distributed exponential
  # random variates. Sorting by this value is mathematically equivalent to weighted sampling
  # without replacement at high efficiency.
  g <- collapse::GRP(d, by = c('version', 'year', 'bg10', 'bg20'))
  d[, N := collapse::fnobs(d[[1L]], g, TRA = "replace")] # Number of households in each year-block group
  d[, ID := .GRP, by = .(version, year, hid)] # # Unique version-year-hid ID
  d[, rand := -log(runif(.N)) / weight] # This efficiently approximates a probability-weighted ordering based on 'weight'
  data.table::setorder(d, N, rand)

  # Total number of version-year-block groups
  n_bg <- data.table::uniqueN(d, by = c('version', 'year', 'bg10', 'bg20'))

  # Original UrbanPop weights by version and county
  # These totals are then enforced in the final output
  d[, county := substr(data.table::fcoalesce(as.character(bg10), as.character(bg20)), 1, 5)]
  w_tot <- d %>%
    collapse::fgroup_by(version, county) %>%
    collapse::fsummarise(county_weight = sum(weight))

  d0 <- data.table::copy(d)
  out <- data.table::data.table()

  # Iterative primary sampling loop
  # WHY: Iteratively assigns available unique households to block groups in order of weighted
  # random priority, peeling off allocated households and block groups until coverage is maximized.
  cli::cli_alert_info(" -- Iterative spatial household assignment loop")
  while (nrow(d) > 0) {

    # Retain the first instance of each year-household
    x <- d[!duplicated(d, by = c("version", "ID"))]

    # Retain the first household within each version-year-block group
    # Since the row-order is probability-weighted, this amounts to a random selection within each year-block groups where the probability of selection is proportional to 'weight'
    x <- x[, .SD[1L], by = .(version, year, bg10, bg20)]

    # Append current iteration result and remove assigned households from 'd'
    out <- rbind(out, x)
    d <- d[!ID %in% x$ID, ]

    # Remove year-block groups from 'd' that are already in 'out'
    d <- d[!out, on = .(version, year, bg10, bg20)]

  }

  # Verify unassigned year-block group rate
  # WHY: Ensures spatial completeness. If too many block groups were missed due to household exhaustion, throw an error
  miss <- d0[!out, on = .(version, year, bg10, bg20)]
  miss_rate <- data.table::uniqueN(miss, by = c('version', 'year', 'bg10', 'bg20')) / n_bg
  if (miss_rate >= 0.00001) cli::cli_abort("UrbanPop unassigned block group rate ({round(miss_rate, 6)}) exceeds threshold.")

  # Secondary assignment for remaining households
  # WHY: Assigns residual households that were not selected in the primary loop.
  d <- d0[!ID %in% out$ID, ]
  data.table::setorder(d, rand)
  x <- d[!duplicated(ID)]
  out <- rbind(out, x)
  rm(d)

  # Adjust weights to match county totals
  # WHY: Post-stratification weight adjustment guarantees that county-level weighted totals
  # match the target UrbanPop control totals exactly after spatial reallocation.
  cli::cli_alert_info(" -- Post-stratifying sample weights at county level...")
  out <- out[w_tot, on = c('version', 'county')]
  out[, w_adj := county_weight[1L] / sum(weight), by = c('version', 'county')]
  out[, weight := as.integer(round(weight * w_adj))]

  # Safety check: All households represented
  if (nrow(out) != data.table::uniqueN(d0, by = c('version', 'year', 'hid'))) {
    cli::cli_abort("UrbanPop safety check failed: Household counts in sample do not match target database.")
  }
  rm(d0)

  upop <- out[, .(year, hid, bg10, bg20, weight)]
  rm(out)
  data.table::setorder(upop, bg10, bg20, year, hid)
  arrow::write_parquet(upop, sink = file.path(release_dir, "location.parquet"), compression = "snappy")

  # --- C. Create Symbolic Links ---
  # PURPOSE: Link shared microdata and donor implicates into the public release directory.
  # WHY: Using symlinks avoids duplicating multi-gigabyte Parquet files in local storage
  # prior to archiving with tar.
  cli::cli_alert_info("Creating symbolic links for public data components...")
  file.symlink(
    from = normalizePath(file.path(target_dir, "dictionary.parquet"), mustWork = TRUE),
    to = file.path(normalizePath(release_dir), "dictionary.parquet")
  )

  for (x in c("ACS_H.parquet", "ACS_P.parquet")) {
    file.symlink(
      from = normalizePath(file.path(target_dir, x), mustWork = TRUE),
      to = file.path(normalizePath(release_dir), x)
    )
  }

  # Link first implicate (M = 1) for all donor surveys
  # WHY: Public releases only include the first implicate (M = 1) for donor surveys to keep
  # public archive sizes manageable while remaining fully usable for point estimation.
  donor_dirs <- list.dirs(target_dir, recursive = FALSE)
  for (x in donor_dirs) {
    m1_path <- file.path(x, "M=1")
    if (dir.exists(m1_path)) {
      out_donor <- file.path(release_dir, basename(x))
      dir.create(out_donor, recursive = TRUE, showWarnings = FALSE)
      file.symlink(
        from = normalizePath(m1_path, mustWork = TRUE),
        to = file.path(normalizePath(release_dir), basename(x), "M=1")
      )
    }
  }

  # --- D. Bundle into .tar Archives ---
  # PURPOSE: Bundle public data files into logical release partitions.
  # WHY: GitHub release asset size limits and download usability require splitting the package:
  # - Part 01: Metadata and crosswalks (dictionary, geography, location)
  # - Part 02: Core ACS household and person microdata (ACS_H, ACS_P)
  # - Part 03: All donor survey implicates (RECS, RECS-P, NHTS, etc.)
  # NOTE: extra_flags = "-h" forces tar to dereference and follow symlinks, archiving the
  # actual files rather than broken link references.
  cli::cli_alert_info("Creating .tar files for upload to Github...")
  old_wd <- getwd()
  setwd(release_dir)
  on.exit(setwd(old_wd), add = TRUE)

  p1 <- c("dictionary.parquet", "geography.parquet", "location.parquet")
  p2 <- c("ACS_H.parquet", "ACS_P.parquet")
  p3 <- grep(".parquet$", list.files(), invert = TRUE, value = TRUE)

  t1 <- paste0("fusionACS_data_", version_date, "_01.tar")
  t2 <- paste0("fusionACS_data_", version_date, "_02.tar")
  t3 <- paste0("fusionACS_data_", version_date, "_03.tar")

  utils::tar(tarfile = t1, files = p1, compression = "none", tar = "tar", extra_flags = "-h")
  utils::tar(tarfile = t2, files = p2, compression = "none", tar = "tar", extra_flags = "-h")
  utils::tar(tarfile = t3, files = p3, compression = "none", tar = "tar", extra_flags = "-h")

  # --- E. Create GitHub Release & Upload ---
  cli::cli_h2("Publishing release {.val {version_date}} to GitHub ({repo})")

  # If desired release already exists, delete first for safety
  # WHY: Prevents tag collision or asset accumulation errors when re-running compilation.
  if (delete_release) {
    piggyback::pb_release_delete(repo = repo, tag = version_date, .token = token)

    # Ensure the release is NOT showing up in the Github API (there is a delay)
    cli::cli_alert_info(" -- Waiting for Github API to update...")
    start <- Sys.time()
    while (version_date %in% piggyback::pb_releases(repo = repo, .token = token)$tag_name) {
      if (difftime(Sys.time(), start, units = "secs") > 90) stop("Timeout after 90s: Deleted release still visible in Github API")
      Sys.sleep(5)
      piggyback::.pb_cache_clear()
    }
  }

  # Create new release on Github
  piggyback::pb_release_create(
    repo = repo,
    tag = version_date,
    name = paste0("fusionACS_data_", version_date),
    body = paste0("Public release of fusionACS pseudo-sample data version ", version_date, "."),
    .token = token
  )

  # Ensure the release is NOT showing up in the Github API (there is a delay)
  cli::cli_alert_info(" -- Waiting for Github API to update...")
  start <- Sys.time()
  while (!version_date %in% piggyback::pb_releases(repo = repo, .token = token)$tag_name) {
    if (difftime(Sys.time(), start, units = "secs") > 90) stop("Timeout after 90s: New release not visible in Github API")
    Sys.sleep(5)
    piggyback::.pb_cache_clear()
  }

  # Upload tar archives
  # WHY: Uploads the three partitioned data bundles as release assets using GitHub LFS via piggyback.
  cli::cli_alert_info("Uploading tar archives to GitHub release...")
  piggyback::pb_upload(t1, repo = repo, tag = version_date, .token = token)
  piggyback::pb_upload(t2, repo = repo, tag = version_date, .token = token)
  piggyback::pb_upload(t3, repo = repo, tag = version_date, .token = token)

  # Clean up unbundled files from public_releases directory
  # WHY: Removes unpacked source/symlinked components, leaving only the compiled tar files in the public_releases directory.
  cli::cli_alert_info("Cleaning up intermediate release files...")
  unlink(p1, recursive = TRUE)
  unlink(p2, recursive = TRUE)
  unlink(p3, recursive = TRUE)

  cli::cli_alert_success("Public release {.val {version_date}} successfully created and uploaded to GitHub!")
  return(invisible(target_dir))
}
