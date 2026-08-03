#' @rdname compileData
#' @aliases NULL
#'
#' @details
#' \strong{\code{compileSpatial()}:}
#' Detects, aggregates, and harmonizes all processed spatial datasets in
#' \code{geo-processed/} into a single, standardized spatial predictor file
#' (\code{geo_predictors.fst}) and spatial dictionary (\code{spatial}).
#'
#' Processes geographic covariates (e.g., land use, walkability, climate) across
#' varying temporal vintages and spatial units into uniform PUMA-level summaries.
#'
#' The workflow proceeds as follows:
#' \itemize{
#'   \item \strong{Spatial Aggregation:} Processes spatial datasets in parallel across CPU
#'     cores via \code{summarizeDataset()}. Matches geographic source geometries to
#'     2010 and 2020 PUMA boundaries using geographic concordances, aggregating metrics
#'     via weighted means (numeric/logical variables) or weighted modes (categorical variables).
#'   \item \strong{Metadata Extraction:} Extracts variable labels, data types, and observed
#'     vintage ranges into a standardized spatial dictionary dataset (\code{spatial}).
#'   \item \strong{Dense Rank Transformation:} Converts numeric predictor variables into dense integer
#'     percentile ranks (\code{data.table::frank(..., ties.method = "dense")}) within each
#'     state-PUMA-vintage grouping prior to expansion.
#'   \item \strong{Temporal Expansion:} Fills temporal gaps across years (2000 through the prior
#'     calendar year) by holding boundary vintages constant.
#'   \item \strong{Unified Storage:} Outer-joins all processed spatial datasets across PUMA-vintages
#'     and exports compressed binary datasets (\code{geo_predictors.fst} and \code{spatial.rda}).
#' }
#'
#' @export

compileSpatial <- function() {

  # Identify all of the *processed.rds files available in /geo-processed
  flist <- list.files(path = "geo-processed", pattern = "_processed.rds$", recursive = TRUE, full.names = FALSE)

  # Determine all available spatial datasets associated with .rds files in 'flist'
  spatial.dsets <- unique(dirname(flist))

  # Print discovery summary to console
  cli::cli_inform(c(
    "i" = "Identified {.val {length(flist)}} processed spatial data file{?s} across {.val {length(spatial.dsets)}} spatial dataset{?s}:",
    "*" = "{spatial.dsets}"
  ))

  # Summarize each spatial dataset (in parallel, if multi-core execution is available)
  cli::cli_inform("Summarizing spatial datasets...")
  result <- pbapply::pblapply(spatial.dsets, summarizeDataset, cl = max(1L, parallel::detectCores() - 1L))
  gc()

  # Temporary recursive merge done only to extract metadata for spatial variable dictionary
  temp <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE), result)
  var.vintages <- map(temp, ~ as.character(sort(unique(temp$vintage[!is.na(.x)]))))
  var.values <- map(temp, ~ {
    if (is.numeric(.x)) {
      numFormat(x = na.omit(.x))
    } else {
      catFormat(.x)
    }
  })

  # Build basic spatial predictor dictionary
  spatial <- labelled::var_label(temp) %>%
    tibble::enframe(name = "predictor", value = "variable_rds") %>%
    mutate(variable_rds = as.character(variable_rds),
           vintage = as.character(var.vintages),
           values = var.values,
           type = map_chr(temp, vctrs::vec_ptype_abbr)) %>%
    filter(!predictor %in% c("vintage", "state", "puma", "puma_vintage"))

  # Save spatial dictionary to package data directory
  cli::cli_inform("Saving geo predictors metadata...")
  use_data2(spatial, overwrite = TRUE)
  rm(temp)

  # Expand each dataset temporally before merging

  # Get the maximum vintage range across all spatial datasets
  years <- unlist(map(result, ~ unique(.$vintage)))
  years <- seq(min(years), max(years))

  expandVintage <- function(d) {

    # Convert numeric values to dense integer ranks within state-PUMA-vintage groupings
    # Ranking prior to temporal expansion significantly reduces processing time
    num_cols <- setdiff(names(which(sapply(d, is.numeric))), key(d))
    d[, (num_cols) := lapply(.SD, frank, ties.method = "dense", na.last = "keep"), by = .(vintage, puma_vintage), .SDcols = num_cols]

    vrng <- range(d$vintage)

    # Front-fill years prior to the earliest available vintage
    front <- d %>%
      filter(vintage == vrng[1]) %>%
      mutate(vintage = list(years[years <= vrng[1]])) %>%
      tidyr::unnest(vintage)

    # Back-fill years after the latest available vintage
    back <- d %>%
      filter(vintage == vrng[2]) %>%
      mutate(vintage = list(years[years >= vrng[2]])) %>%
      tidyr::unnest(vintage)

    # Retain intermediate years as-is
    middle <- d %>%
      filter(!vintage %in% vrng)

    # Combine temporally expanded partitions and re-key data.table
    rbind(front, middle, back) %>%
      distinct() %>%
      mutate(vintage = as.character(vintage)) %>%
      data.table(key = c('vintage', 'state', 'puma', 'puma_vintage'))

  }

  result <- map(result, expandVintage)

  # Recursively merge individual processed spatial datasets into a master table
  result <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE), result)

  # Convert remaining character columns to unordered factors for storage efficiency
  result <- mutate_if(result, is.character, as.factor)

  # Save final compiled spatial predictors table to disk
  cli::cli_inform("Writing {.path geo-processed/geo_predictors.fst} to disk...")
  fst::write_fst(result, path = "geo-processed/geo_predictors.fst", compress = 95)

}

# Internal helper function to aggregate individual spatial datasets to PUMA level
summarizeDataset <- function(dataset) {

  # Identify processed .rds files for the target spatial dataset
  flist <- list.files(path = file.path("geo-processed", dataset), pattern = "_processed.rds$", recursive = TRUE, full.names = TRUE)

  # Load and process each file in the spatial dataset directory
  data <- lapply(flist, function(x) {

    # Load raw data table
    d <- as.data.table(readRDS(x))

    # Validate that vintage values follow recognized formats (e.g., 'always', YYYY, or YYYY-YYYY)
    ok <- all(
      d$vintage == "always" |
        d$vintage %in% 1900:as.integer(substring(Sys.Date(), 1, 4)) |
        grepl("^\\d{4}-\\d{4}$", d$vintage)
    )
    if (any(!ok)) stop("The following input .rds file has invalid 'vintage' values:\n", x)

    # Select the optimal geographic concordance file matching input spatial geometry
    glist <- list.files("geo-processed/concordance", pattern = "^geo_concordance.*\\.fst$", full.names = TRUE)
    vars <- map(glist, ~ intersect(names(fst(.x)), names(d)))
    i <- which.max(lengths(vars))
    gdonor <- vars[[i]]

    # Load geographic concordance and normalize spatial area weights to prevent integer overflow
    pcord <- fst::fst(glist[i])
    gtarget <- intersect(c("state", "puma10", "puma20"), names(pcord))
    gv <- unique(c(gtarget, gdonor))
    pcord <- pcord[c('puma_weight', gv)] %>%
      na.omit() %>%
      setnames(c("W", gv)) %>%
      mutate(W = W / mean(W)) %>%
      data.table(key = gv)

    # Aggregate spatial weights across matching geometries
    pcord <- pcord[, .(W = sum(W)), by = gv]

    # Identify non-geographic predictor columns to aggregate
    sumvars <- setdiff(names(d), c("vintage", gdonor))

    # Merge spatial data with concordance table on matching donor geography
    d <- d[pcord, on = intersect(gdonor, names(d)), allow.cartesian = TRUE]
    d <- d[!is.na(d$vintage), ]

    # Merge with PUMA crosswalk to map source data to both 2010 and 2020 PUMA boundaries
    data("puma_crosswalk", package = "fusionData")
    d <- d[puma_crosswalk, on = intersect(gtarget, names(puma_crosswalk)), allow.cartesian = TRUE]
    d <- d[!is.na(d$vintage), ]

    # Reshape table to long format separating 2010 and 2020 PUMA vintages
    dlong <- melt(
      data = d,
      id.vars = c("state", "vintage", "W", "xwalk_weight", sumvars),
      measure.vars = c("puma10", "puma20"),
      variable.name = "puma_vintage",
      value.name = "puma"
    )

    # Standardize PUMA vintage labels to integer years (2010 or 2020)
    dlong[, puma_vintage := fifelse(puma_vintage == "puma10", 2010L, 2020L)]

    # Aggregation summary function:
    # Computes area-weighted mean for numeric/logical variables and weighted mode for categorical variables
    sumFun <- function(x, w) {
      if (is.ordered(x)) x <- as.integer(x)
      if (is.numeric(x) | is.logical(x)) {
        weighted.mean(x, w, na.rm = TRUE)
      } else {
        as.character(collapse::fmode(x = x, w = w, na.rm = TRUE))
      }
    }

    # Derive PUMA-level summary values weighted by area intersection and geographic weight
    result <- dlong[, lapply(.SD, sumFun, w = W * xwalk_weight), by = .(vintage, state, puma, puma_vintage), .SDcols = sumvars]

    return(result)

  })

  # Expand static 'always' vintages to cover full historical range (2000 through prior calendar year)
  allowed.years <- 2000:(as.integer(format(Sys.Date(), "%Y")) - 1)
  data <- map(data, ~ mutate(., vintage = ifelse(vintage == "always", paste(range(allowed.years), collapse = "-"), vintage)))

  # Expand year range strings (e.g., "2015-2020") into sequence of individual annual vintages
  expandRange <- function(d) {
    V <- unique(d$vintage)
    if (any(grepl("-", V))) {
      v <- ifelse(grepl("-", V), map(V, ~ as.character(eval(parse(text = sub("-", ":", .x, fixed = TRUE))))), V)
      d$vintage <- v[match(d$vintage, V)]
      d <- tidyr::unnest(d, vintage)
      d <- as.data.table(d)
    }
    d$vintage <- as.integer(d$vintage)
    return(d)
  }
  data <- map(data, expandRange)

  # Restrict dataset records to allowed annual vintages
  data <- map(data, ~ filter(., vintage %in% allowed.years))

  # Extract and merge shared predictor columns across files to avoid duplicate variable names
  exclude <- c("vintage", "state", "puma", "puma_vintage")
  cols_list <- lapply(data, function(dt) setdiff(names(dt), exclude))
  shared_cols <- names(which(table(unlist(cols_list)) > 1))
  if (length(shared_cols)) {
    keep <- c(exclude, shared_cols)
    temp <- rbindlist(lapply(data, function(dt) dt[, ..keep]), use.names = TRUE, fill = TRUE)
    data <- lapply(data, function(dt) dt[, (shared_cols) := NULL])
    data <- c(data, list(temp))
    rm(temp)
  }

  # Outer-join processed predictor subsets by geographic keys
  result <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE, by = exclude), data)

  # Construct standardized, syntactically valid predictor names using the 'dataset..variable' convention
  gvars <- setdiff(names(result), exclude)
  vnames <- make.names(paste(tolower(dataset), betterAbbreviate(gvars), sep = ".."), unique = TRUE)
  vlabs <- setNames(as.list(gvars), vnames)
  names(result) <- c(exclude, vnames)
  result <- labelled::set_variable_labels(result, .labels = vlabs)

  # Set data.table primary key for downstream joins and return
  setkeyv(result, exclude)

  return(result)

}
