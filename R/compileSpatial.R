#' Compile all spatial predictor variables
#'
#' @description
#' Detects and compiles all processed spatial datasets located in \code{/geo-processed} into a single .fst file to be utilized by \link{prepare}.
#'
#' @return Saves \code{/geo-processed/geo_predictors.fst} to disk.
#'
#' @examples
#' compileSpatial()
#'
#' @export

#-----

compileSpatial <- function() {

  # Identify all of the *processed.rds files available in /geo-processed
  flist <- list.files(path = "geo-processed", pattern = "_processed.rds$", recursive = TRUE, full.names = FALSE)

  # Determine all available spatial datasets associated with .rds files in 'flist'
  spatial.dsets <- unique(dirname(flist))

  # Print message to console
  cat("Identified", length(flist), "processed.rds files across", length(spatial.dsets), "spatial datasets:", paste(spatial.dsets, collapse = ", "), "\n")

  # Summarize each spatial dataset (in parallel, if possible)
  cat("Summarizing spatial datasets...\n")
  result <- pbapply::pblapply(spatial.dsets, summarizeSpatialDataset, cl = max(1L, parallel::detectCores() - 1L))

  # Troubleshooting; identify which spatial dataset is giving trouble
  #for (v in spatial.dsets) summarizeSpatialDataset(v)

  # Merge the individual data frames in 'data' (on keyed variables set by summarizeSpatialDataset)
  result <- Reduce(function(...) merge(..., all = TRUE), result)

  # Remove columns with no variation
  keep <- !sapply(result, novary)
  keep[c('state', 'puma10', 'vintage')] <- TRUE
  result <- result[, ..keep]

  # Coerce character variables to unordered factor
  # Any truly ordered factors should be made so in the upstream script that generates the assocaited "*_processed.rds" file
  # Extract the var_labels so they can be reassigned after factor coercion (dropped by coercion step)
  vlabs <- labelled::var_label(result)
  result <- mutate_if(result, is.character, factor)
  result <- labelled::set_variable_labels(result, .labels = vlabs)

  # Remove columns with large number of levels (>30; HARD CODED!)
  # This helps prevent factor variables producing excessive dummy variables in train() and fuse()
  keep <- map_lgl(result, ~ length(levels(.x)) <= 30)
  keep[c('state', 'puma10', 'vintage')] <- TRUE
  result <- result[, ..keep]

  # Ensure 'result' is sorted properly
  setorder(result, state, puma10, vintage)

  #-----

  # Extract spatial variable metadata

  # Vintages available for each spatial predictor
  var.vintages <- result %>%
    map(~ as.character(sort(unique(result$vintage[!is.na(.x)]))))

  # Variable summaries
  var.values <- result %>%
    map_chr(~ if (is.numeric(.x)) {numFormat(x = na.omit(.x))} else {fctFormat(.x)})

  # Basic spatial predictor dictionary
  spatial <- labelled::var_label(result) %>%
    enframe(name = "predictor", value = "variable_rds") %>%
    mutate(variable_rds = as.character(variable_rds),
           vintage = as.character(var.vintages),
           values = var.values,
           type = map_chr(result, vctrs::vec_ptype_abbr)) %>%
    filter(!predictor %in% c("state", "puma10", "vintage"))

  # Save spatial dictionary to disk
  usethis::use_data(spatial, overwrite = TRUE)

  #-----

  # Variable summaries
  # var.values <- result %>%
  #   select(-any_of(c(gtarget, 'vintage'))) %>%  # Remove geo target variables and vintage
  #   map_chr(~ if (is.numeric(.x)) {numFormat(x = .x)} else {fctFormat(.x)})

  # Save final result to disk
  cat("Writing 'geo_predictors.fst' to disk...\n")
  fst::write_fst(result, path = "geo-processed/geo_predictors.fst", compress = 100)

}

#-----------------------------------

# Example usage
# summarizeSpatialDataset("climate")
# summarizeSpatialDataset("IRS-SOI")

# Underlying function to process a specified dataset
summarizeSpatialDataset <- function(dataset) {

  # Check for valid 'dataset' argument
  stopifnot(dataset %in% basename(list.dirs("geo-processed", recursive = FALSE)))

  # Soft load the puma_concordance.fst file
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

  # Variables in puma_concordance defining the "target" geography (i.e. uniquely-identified PUMA's)
  gtarget <- c("state", "puma10")

  # The PUMA-related weight variable in 'pcord' (i.e. housing unit count)
  gw <- "puma_weight"

  gv <- unique(c(gtarget, gdonor))
  pcord <- pcord[c(gw, gv)] %>%
    na.omit() %>%   # Removes an entries where the 'gdonor' variables might be missing/incomplete
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
    d <- d[!is.na(vintage), ]

    # Summary function; handles numeric and categorical cases
    # Returns weighted mean in the numeric case
    # Returns the single most common value/level in the categorical case
    sumFun <- function(x, w) {
      if (is.numeric(x)) {
        weighted.mean(x, w, na.rm = TRUE)
      } else {
        tab <- stats::xtabs(w ~ x)
        out <- names(sort(tab, decreasing = TRUE))[1]
        ifelse(is.logical(x), as.logical(out), out)
      }
    }

    # Calculate weighted mean for each summary variable at the PUMA-vintage level
    d[, lapply(.SD, sumFun, w = W), by = c(gtarget, "vintage"), .SDcols = sumvars] %>%
      mutate_at(intvars, ~ as.integer(round(.x))) %>%
      mutate_if(is.double, cleanNumeric, tol = 0.001)

  }

  # Apply summarizePUMA() to each individual data frame in 'data'
  data <- lapply(data, summarizePUMA)

  # Testing with 'vintage' set to a range
  #data[[1]]$vintage[1:5] <- "1980-1985"

  #---

  # Parse any 'vintage' values that express a year range (e.g. "2015-2020")
  expandRange <- function(d) {
    V <- unique(d$vintage)
    if (any(grepl("-", V))) {
      v <- ifelse(grepl("-", V), map(V, ~ as.character(eval(parse(text = sub("-", ":", .x, fixed = TRUE))))), V)
      d$vintage <- v[match(d$vintage, V)]  # This is rather slow...
      d <- unnest(d, vintage)
    }
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
    result <- Reduce(function(...) merge(..., all = TRUE), data)

  } else {

    result$`_id` <- NULL

  }

  #-----

  # Create the abbreviated variables names for spatia predictors
  # By convention, these use ".." to separate the spatial dataset identifier from the variable abbreviation
  # NOTE that make.names() will remove dashed spatial spatial dataset name (e.g. "EPA-SLD" becomes "epa.sld")
  # Forcing syntactically valid names avoids issues in rpart()
  vnames <- make.names(paste(tolower(dataset), betterAbbreviate(gvars), sep = ".."), unique = TRUE)
  vlabs <- setNames(as.list(gvars), vnames)
  names(result) <- c(gtarget, 'vintage', vnames)
  result <- labelled::set_variable_labels(result, .labels = vlabs)

  #-----

  # Assemble final data.table result
  # Pre-keyed for subsequent merges
  result <- result %>%
    mutate(vintage = as.character(vintage)) %>%
    data.table(key = c(gtarget, "vintage"))

  return(result)

}
