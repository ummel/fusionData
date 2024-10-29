#' Compile all spatial predictor variables
#'
#' @description
#' Detects and compiles all processed spatial datasets located in \code{/geo-processed} into a single \code{geo_predictors.fst} file to be utilized by \link{assemble}.
#'
#' @return Saves \code{/geo-processed/geo_predictors.fst} to disk.
#'
#' @examples
#' compileSpatial()
#'
#' @export

#-----

compileSpatial <- function() {

  # Variables in geo_concordance defining the "target" geography (i.e. uniquely-identified PUMA's)
  gtarget <- c("state", "puma10")

  # Identify all of the *processed.rds files available in /geo-processed
  flist <- list.files(path = "geo-processed", pattern = "_processed.rds$", recursive = TRUE, full.names = FALSE)

  # Determine all available spatial datasets associated with .rds files in 'flist'
  spatial.dsets <- unique(dirname(flist))

  # Print message to console
  cat("Identified", length(flist), "processed.rds spatial data files across", length(spatial.dsets), "spatial datasets:\n", paste(spatial.dsets, collapse = ", "), "\n")

  #---

  # Summarize each spatial dataset (in parallel, if possible)
  # NOTE: Should this be done once for each dataset when it is created in /geo-processed? To prevent re-generating unnecessarily?
  cat("Summarizing spatial datasets...\n")
  result <- pbapply::pblapply(spatial.dsets, summarizeSpatialDataset, cl = max(1L, parallel::detectCores() - 1L))

  # Troubleshooting; identify which spatial dataset is giving trouble
  #for (v in spatial.dsets) summarizeSpatialDataset(v)

  #---

  # Extract spatial variable metadata
  temp <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE), result)

  # Vintages available for each spatial predictor
  var.vintages <- temp %>%
    map(~ as.character(sort(unique(result$vintage[!is.na(.x)]))))

  # Variable summaries
  var.values <- temp %>%
    map_chr(~ if (is.numeric(.x)) {numFormat(x = na.omit(.x))} else {fctFormat(.x)})

  # Basic spatial predictor dictionary
  spatial <- labelled::var_label(temp) %>%
    tibble::enframe(name = "predictor", value = "variable_rds") %>%
    mutate(variable_rds = as.character(variable_rds),
           vintage = as.character(var.vintages),
           values = var.values,
           type = map_chr(temp, vctrs::vec_ptype_abbr)) %>%
    filter(!predictor %in% c("state", "puma10", "vintage"))

  # Save spatial dictionary to disk
  usethis::use_data(spatial, overwrite = TRUE)
  rm(temp)

  #---

  # Expand each dataset temporally before merging
  # EXPLAIN...

  # Get the maximum vintage range across all spatial datasets
  years <- unlist(map(result, ~ unique(.$vintage)))
  years <- seq(min(years), max(years))

  expandVintage <- function(d) {

    vrng <- range(d$vintage)

    front <- d %>%
      filter(vintage == vrng[1]) %>%
      mutate(vintage = list(years[years <= vrng[1]])) %>%
      tidyr::unnest(vintage)

    back <- d %>%
      filter(vintage == vrng[2]) %>%
      mutate(vintage = list(years[years >= vrng[2]])) %>%
      tidyr::unnest(vintage)

    middle <- d %>%
      filter(!vintage %in% vrng)

    rbind(front, middle, back) %>%
      distinct() %>%
      mutate(vintage = as.character(vintage)) %>%
      data.table(key = c(gtarget, "vintage"))

  }

  result <- map(result, expandVintage)

  #---

  # Merge the individual data tables in 'result' (on keyed variables set by summarizeSpatialDataset)
  result <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE), result)

  # Remove columns with no variation
  keep <- !sapply(result, novary)
  keep[c('state', 'puma10', 'vintage')] <- TRUE
  result <- result[, ..keep]

  # Coerce character variables to unordered factor
  # Any truly ordered factors should be made so in the upstream script that generates the associated "*_processed.rds" file
  # Extract the var_labels so they can be reassigned after factor coercion (dropped by coercion step)
  # vlabs <- labelled::var_label(result)
  # result <- mutate_if(result, is.character, factor)
  # result <- labelled::set_variable_labels(result, .labels = vlabs)

  # Remove columns with large number of levels (>30; HARD CODED!)
  # This helps prevent factor variables producing excessive dummy variables in train() and fuse()
  # keep <- map_lgl(result, ~ length(levels(.x)) <= 30)
  # keep[c('state', 'puma10', 'vintage')] <- TRUE
  # result <- result[, ..keep]

  # Ensure 'result' is sorted properly
  #setorder(result, state, puma10, vintage)

  #-----

  # Could impute -- takes awhile. Not a ton of missing values
  #result <- fusionModel::impute(data = result, ignore = "puma10")

  # Convert numeric values to integer ranks, when possible
  # mutate_of() will ignore 'vintage' grouping variable
  result <- result %>%
    group_by(vintage) %>%
    mutate_if(is.numeric, frank, ties.method = "dense", na.last = "keep") %>%
    setDT(key = c(gtarget, "vintage"))

  #-----

  # Variable summaries
  # var.values <- result %>%
  #   select(-any_of(c(gtarget, 'vintage'))) %>%  # Remove geo target variables and vintage
  #   map_chr(~ if (is.numeric(.x)) {numFormat(x = .x)} else {fctFormat(.x)})

  # Save final result to disk
  cat("Writing 'geo_predictors.fst' to disk...\n")
  fst::write_fst(result, path = "geo-processed/geo_predictors.fst", compress = 80)

}

#-----------------------------------

# Example usage
# summarizeSpatialDataset("climate")
# summarizeSpatialDataset("IRS-SOI")

# Underlying function to process a specified dataset
summarizeSpatialDataset <- function(dataset) {

  # Variables in geo_concordance defining the "target" geography (i.e. uniquely-identified PUMA's)
  gtarget <- c("state", "puma10")

  # Check for valid 'dataset' argument
  stopifnot(dataset %in% basename(list.dirs("geo-processed", recursive = FALSE)))

  # Soft load the geo_concordance.fst file
  pcord <- fst::fst("geo-processed/concordance/geo_concordance.fst")

  #---

  # Identify the *processed.rds files available for the specified 'dataset'
  flist <- list.files(path = file.path("geo-processed", dataset), pattern = "_processed.rds$", recursive = TRUE, full.names = TRUE)

  # Load the .rds files as a list
  data <- lapply(flist, readRDS)

  #---

  # Check for valid 'vintage' values in input data
  ok <- map_lgl(data, ~ all(
    .x$vintage == "always" |
      .x$vintage %in% 1900:as.integer(substring(Sys.Date(), 1, 4)) |
      grepl("^\\d{4}-\\d{4}$", .x$vintage)  # Basic check for something like "dddd-dddd" (e.g. "2015-2020")
  ))
  if (any(!ok)) stop("The following input .rds files have invalid 'vintage' values:\n", paste(flist[!ok], collapse = "\n"))

  #---

  # Weird test case for dataset = "IRS-SOI"
  #data[[5]] <- tibble(zcta10 = data[[4]]$zcta10, vintage = 1900, Something = runif(nrow(data[[4]])))

  #---

  # Unique variables names in 'data'
  dnames <- unique(unlist(map(data, names)))

  # Identify geographic variables in 'd'
  gdonor <- intersect(names(pcord), dnames)

  # Convert each element of 'data' to a keyed data.table
  data <- map(data, ~ data.table(.x, key = intersect(names(.x), gdonor)))

  #---

  # The PUMA-related weight variable in 'pcord' (i.e. housing unit count)
  gw <- "puma_weight"

  gv <- unique(c(gtarget, gdonor))
  pcord <- pcord[c(gw, gv)] %>%
    na.omit() %>%   # Removes any entries where the 'gdonor' variables might be missing/incomplete
    setnames(c("W", gv)) %>%  # Rename the 'gw' variable to "W" for ease of use in data.table operations
    mutate(W = W / mean(W)) %>%   # Just to avoid integer overflow issues
    data.table(key = gv)

  # Aggregate geographic weight
  pcord <- pcord[, .(W = sum(W)), by = gv]

  #---

  # The variables to be summarized at PUMA level
  gvars <- setdiff(dnames, c("vintage", gdonor))

  summarizePUMA <- function(d) {

    # Variables to summarize
    sumvars <- intersect(gvars, names(d))

    # Integer summary variables
    intvars <- sumvars[map_lgl(d[, ..sumvars], is.integer)]

    # Merge 'd' and 'pcord' on the common spatial variable(s)
    d <- d[pcord, on = intersect(gdonor, names(d)), allow.cartesian = TRUE]
    d <- d[!is.na(d$vintage), ]

    # Summary function; handles numeric and categorical cases
    # Returns weighted mean in the numeric case
    # Returns the single most common value/level in the categorical case
    sumFun <- function(x, w) {
      if (is.numeric(x)) {
        weighted.mean(x, w, na.rm = TRUE)
      } else {
        tab <- table2(x, w)
        m <- names(sort(tab, decreasing = TRUE))[1]  # Modal value
        m <- ifelse(m == "NA", NA, m)
        ifelse(is.logical(x), as.logical(m), m)
      }
    }

    # Calculate weighted mean for each summary variable at the PUMA-vintage level
    d[, lapply(.SD, sumFun, w = W), by = c(gtarget, "vintage"), .SDcols = sumvars]
    #mutate_at(intvars, ~ as.integer(round(.x))) %>%
    #mutate_if(is.double, cleanNumeric, tol = 0.001)

  }

  # Apply summarizePUMA() to each individual data frame in 'data'
  data <- lapply(data, summarizePUMA)

  #---

  # Replace 'always' vintage values with maximum range starting from year 2000 and ending in current year (e.g. 2000-2024)
  allowed.years <- 2000:(as.integer(format(Sys.Date(), "%Y")) - 1)
  data <- map(data, ~ mutate(., vintage = ifelse(vintage == "always", paste(range(allowed.years), collapse = "-"), vintage)))

  # Parse any 'vintage' values that express a year range (e.g. "2015-2020")
  expandRange <- function(d) {
    V <- unique(d$vintage)
    if (any(grepl("-", V))) {
      v <- ifelse(grepl("-", V), map(V, ~ as.character(eval(parse(text = sub("-", ":", .x, fixed = TRUE))))), V)
      d$vintage <- v[match(d$vintage, V)]  # This is rather slow...
      d <- tidyr::unnest(d, vintage)
    }
    #d$vintage <- as.character(d$vintage)  # Force 'vintage' to be character (even if input is un-expanded integer)
    d$vintage <- as.integer(d$vintage)
    return(d)
  }

  data <- map(data, expandRange)

  #---

  # Determine which list elements in 'data' can be safely 'rbind'-ed (appended) and which need to be merged
  result <- rbindlist(data, use.names = TRUE, fill = TRUE, idcol = "_id")

  # If a variable is present in more than one 'data' element, then it is treated as available for all (i.e. rbind candidate)
  check <- result[, lapply(.SD, function(x) !all(is.na(x))), by = "_id", .SDcols = gvars] %>%
    mutate_if(is.logical, ~ if (sum(.x) > 1) {TRUE} else {.x})

  # List identifying groups of 'data' elements that can be safely 'rbind'-ed (appended)
  grps <- split(x = check[["_id"]], f = check[, ..gvars], drop = TRUE)

  #-----

  # rbind 'data' elements whenever possible and then merge remaining elements on 'gtarget' and vintage
  if (length(grps) > 1) {

    data <- map(grps, ~ rbindlist(data[.x], use.names = TRUE, fill = TRUE))

    # Convert each data frame in 'data' to a keyed data table to enable fast merge (below)
    data <- map(data, ~ data.table(.x, key = c(gtarget, 'vintage')))

    # Merge the individual data frames in 'data' (on keyed variables set in previous step)
    result <- Reduce(function(...) data.table::merge.data.table(..., all = TRUE), data)

  } else {

    result$`_id` <- NULL

  }

  #-----

  # Create the abbreviated variables names for spatial predictors
  # By convention, these use ".." to separate the spatial dataset identifier from the variable abbreviation
  # NOTE that make.names() will remove dashed spatial spatial dataset name (e.g. "EPA-SLD" becomes "epa.sld")
  # Forcing syntactically valid names avoids potential downstream issues
  vnames <- make.names(paste(tolower(dataset), betterAbbreviate(gvars), sep = ".."), unique = TRUE)
  vlabs <- setNames(as.list(gvars), vnames)
  names(result) <- c(gtarget, 'vintage', vnames)
  result <- labelled::set_variable_labels(result, .labels = vlabs)

  #-----

  # Assemble final data.table result
  # Pre-keyed for subsequent merges
  result <- result %>%
    filter(vintage %in% allowed.years) %>%   # !!! Restricts the vintages that will be returned (i.e. 2000 or later)
    mutate_if(is.character, as.factor) %>% # Necessary? Should really be done closer to the source data
    mutate(vintage = as.character(vintage)) %>% # Necessary?
    data.table(key = c(gtarget, "vintage"))

  #-----

  # # TO DO: Interpolate missing values temporally, by 'gtarget', if possible
  # interpolate_vector <- function(x, v) {
  #   if (any(is.finite(x))) {
  #     if (novary(x)) {
  #       rep(unique(na.omit(x)), length(v))
  #     } else {
  #       if (is.numeric(x)) {
  #         approx(v, x, xout = v, rule = 2)$y
  #       } else {
  #         tab <- table2(x, na.rm = TRUE)
  #         m <- names(sort(tab, decreasing = TRUE))[1]  # Modal value
  #         x[is.na(x)] <- ifelse(is.logical(x), as.logical(m), m)
  #         x
  #       }
  #     }
  #   } else {
  #     x
  #   }
  # }
  #
  # d <- filter(result, state == "01", puma10 == "00100") %>%
  #   select(-state, -puma10)
  #
  # interpolate_block <- function(d) {
  #   mutate_at(d, -1L, interpolate_vector, v = d$vintage)
  # }
  #
  # test <- temp[, interpolate_block(.SD), by = gtarget]

  #-----

  return(result)

}
