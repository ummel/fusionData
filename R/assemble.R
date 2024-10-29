#' Assemble data used for survey fusion
#'
#' @description
#' Assembles data inputs to pass to \code{\link[fusionModel]{train}} and \code{\link[fusionModel]{fuse}} to perform survey fusion. Adds fusion, replicate weight, and/or spatial variables and checks that donor and recipient output data frames are consistent.
#'
#' @param x List object produced by \code{\link{prepare}}.
#' @param fusion.variables Character. Names of donor variables to be included in output as fusion candidates. If NULL (default), an attempt is made to return all donor variables not used in predictor harmonization process.
#' @param spatial.datasets Character. Vector of requested spatial datasets to merge (e.g. `"EPA-SLD"`) or either of two special values: `"all"` (default) or `"none"`.
#' @param window Integer. Size of allowable temporal window, in years, when merging spatial variables. `window = 0` (default) means that a spatial variable is only included if it has the same vintage as the survey. See Details.
#' @param pca Numeric. Controls whether/how PCA is used to reduce dimensionality of spatial variables. Default (NULL) is no PCA. If non-NULL, should be a numeric vector of length two; e.g. \code{pca = c(50, 0.95)}. First number is the maximum number of components to return; second number is target proportion of variance explained. See Details.
#' @param replicates Logical. Should replicate observation weights be included, if available? Defaults to FALSE.
#' @param agg_fun List. See \code{\link{fusionInput}}.
#' @param agg_adj List. See \code{\link{fusionInput}}.
#'
#' @details Spatial variables are included if the associated vintage is within +/- `window` years of the survey vintage. In cases where the spatial variable has multiple vintages equidistant from the survey vintage, the older vintage is selected. Variables with `vintage = "always"` are, of course, always included.
#'
#' @details PCA is restricted to numeric spatial variables and is computed using \code{\link[stats]{prcomp}}. The returned number of principal components is the lesser of \code{pca[1]} or the number of components that explain at least \code{pca[2]} proportion of the variance. For example, \code{pca = c(50, 0.95)} will select the fewest number of components that explain 95% of the variance, up to 50 components maximum. NA's in numeric spatial variables are imputed using median value prior to computing the principal components.
#'
#' @return A list of length two containing donor and recipient microdata to pass to \code{\link[fusionModel]{train}} and \code{\link[fusionModel]{fuse}}.
#'
#' @examples
#' prep <- prepare(donor = "RECS_2015",
#'                 recipient = "ACS_2015",
#'                 respondent = "household",
#'                 implicates = 3)
#'
#' data <- assemble(x = prep)
#'
#' @export

#-----

# library(tidyverse)
# library(data.table)
# x <- readRDS("asec_prepare_out.rds")
# fusion.variables = c("heatsub", "heatval", "incchild", "kidcneed", "hipval", "spmwic", "adjginc")
# spatial.datasets = "all"
# window = 2
# pca = NULL
# replicates = FALSE
# agg_adj <- list(spmwic = ~if_else(duplicated(data.table(asec_2019_hid, spmfamunit)), 0, spmwic))
# agg_fun <- list(spmwic = "mean")

#-----

# NOTE: See custom aggregation functions at bottom of script.
assemble <- function(x,
                     fusion.variables = NULL,
                     spatial.datasets = "all",
                     window = 2,
                     pca = NULL,
                     replicates = FALSE,
                     agg_fun = NULL,
                     agg_adj = NULL) {

  # Names of the donor and recipient surveys
  donor <- names(x$harmonized)[1]
  recipient <- names(x$harmonized)[2]

  # Respondent identifier variables in donor and recipient (possibly including 'pid')
  did <- attr(x$harmonized[[1]], "identifier")
  rid <- attr(x$harmonized[[2]], "identifier")

  # Respondent type
  respondent <- ifelse("pid" %in% did, "person", "household")

  # Names of the location variables ("loc..*")
  lvars <- attr(x$location, "location.vars")

  # Names of the recipient geographic variables
  gtarget <- attr(x$harmonized[[2]], "geo.vars")

  #-----

  # Identify spatial variables and datasets available in 'geo_predictors.fst'
  geo <- fst::fst("geo-processed/geo_predictors.fst")
  gvars <- setdiff(names(geo), c(gtarget, 'vintage'))
  gsets <- unique(map_chr(strsplit(gvars, "..", fixed = TRUE), 1L))

  #-----

  # Validate arguments
  stopifnot({
    respondent %in% c("household", "person")
    spatial.datasets[1] %in% c("all", "none") | all(spatial.datasets %in% gsets)
    window >= 0 & window %% 1 == 0
    is.null(pca) | (is.numeric(pca) & length(pca) == 2)
    is.logical(replicates)
  })

  #-----

  # Restrict 'gvars' and 'gsets', if requested by 'spatial.datasets'
  if (length(spatial.datasets) > 1) {
    gvars <- unlist(map(spatial.datasets, ~ gvars[str_starts(gvars, fixed(paste0(.x, "..")))]))
    gsets <- spatial.datasets
  }

  #-----

  # Determine which donor fusion variables to load from disk
  cat("Identifying donor fusion variables...\n")

  # Load requested respondent level microdata
  fpath <- list.files(path = "survey-processed", pattern = paste(donor, ifelse(respondent == "household", "H", "P"), "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
  d1 <- fst::fst(fpath)

  # If respondent = "household", attempt to load person-level microdata as well (NULL if unavailable or unnecessary)
  fpath <- list.files(path = "survey-processed", pattern = paste(donor, ifelse(respondent == "household", "P", NULL), "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
  d2 <- if (length(fpath)) fst::fst(fpath) else NULL

  # All potential donor variable names
  dnames <- unique(c(names(d1), names(d2)))

  # Names of replicate weight variables
  rvars <- grep("^rep_\\d+$", dnames, value = TRUE)

  # Identify the fusion variables
  disallowed <- unique(c(did, rvars, "pid", "weight", gtarget, attr(x$location, "intersection.vars")))  # Variables that are not feasible fusion candidates
  employed <- attr(x$harmonized[[donor]], "employed.vars")  # Donor variables used in harmonization

  # If no fusion variables specified, return a plausible set
  # Exclude disallowed variables and those used for harmonization
  if (is.null(fusion.variables)) fusion.variables <- setdiff(names(d), c(disallowed, employed))

  # Check for invalid/problematic fusion variables and report to console
  invalid1 <- setdiff(fusion.variables, dnames)
  if (length(invalid1) > 0) cat("WARNING: Omitted fusion variables; some are not in the donor microdata:\n", paste(invalid1, collapse = ", "), "\n")
  invalid2 <- intersect(fusion.variables, disallowed)
  if (length(invalid2) > 0) cat("WARNING: Removed fusion variables; some are not feasible candidates:\n", paste(invalid2, collapse = ", "), "\n")

  # Detect case where a variable used in harmonization is requested for fusion
  # In this case, remove the associated
  invalid3 <- intersect(fusion.variables, employed)
  if (length(invalid3) > 0) cat("WARNING: Some fusion variables were used to construct harmonized predictors:\n", paste(invalid3, collapse = ", "), "\n")

  # Identify harmonized variables to 'drop' from microdata
  # This occurs if there is a conflict with a fusion variable; the fusion variables is retained and affected harmonized predictor removed (below)
  hvars <- attr(x$harmonized, "harmonized.vars")
  drop <- lapply(invalid3, function(x) grep(paste0("^", x, "__"), hvars, value = TRUE))
  drop <- unique(unlist(drop))
  if (length(drop) > 0) cat("WARNING: Removed harmonized predictors that conflict with fusion variables:\n", paste(drop, collapse = ", "), "\n")
  hvars <- setdiff(hvars, drop)  # Updated names of harmonized variables

  # Remove 'invalid1' and 'invalid2' from fusion variables but keep variables involved in harmonies (with Warning above)
  fvars <- setdiff(fusion.variables, c(invalid1, invalid2))

  # Report which variables are being added as fusion variables
  fvars <- sort(fvars)
  cat("Including the following fusion variables:\n", paste(fvars, collapse = ", "), "\n")

  #-----

  # Load necessary donor variables from disk
  rvars <- if (replicates) rvars else NULL  # Replicate weight variables
  avars <- unique(unlist(lapply(agg_adj, all.vars)))  # Variables used in the "adjustment" formulae
  keep <- unique(c(did, "pid", fvars, avars, rvars))

  # Load required data from disk
  d1 <- d1[intersect(names(d1), keep)]
  d2 <- d2[c(did, intersect(names(d2), setdiff(keep, names(d1))))]

  # Person-level fusion variables to be aggregated
  pvars <- intersect(fvars, names(d2))

  #-----

  # Aggregate person-level fusion variables to household level, if necessary
  if (length(pvars)) {

    # Create merged 'd2' object for aggregation to household-level
    d2v <- unique(c(did, "pid", avars, pvars))
    d2 <- full_join(d1, d2, by = did) %>%
      select(all_of(d2v)) %>%
      as.data.table()

    # Ensure rows are ordered by household ID and then 'pid'
    # This is necessary for the  ref() aggregation function to work as intended (i.e. return obs. for pid = 1 in first position)
    setorderv(d2, cols = c(did, "pid"))

    # Apply any custom modification code to 'd2' before aggregation
    cat("Applying pre-aggregation adjustment code:\n")
    for (i in seq_along(agg_adj)) {
      v <- names(agg_adj)[i]
      f <- as.character(agg_adj[[i]])[2]
      cat(" ", paste(v, "=", f), "\n")
      d2 <- mutate(d2, !!v := !!rlang::parse_quo(f, env = rlang::current_env()))
    }

    # Create aggregation function call for each person-level fusion variable
    cat("Using following aggregation functions for person-level variables:\n")
    pcalls <- lapply(pvars, function(v) {
      cl <- class(d2[[v]])[1]
      f <- switch(cl,
                  numeric = "sum",
                  factor = "ref",
                  ordered = "max")

      # Check that the data class  of 'v' is valid
      # Should be numeric or a factor (possibly ordered)
      if (is.null(f)) stop("Invalid data class (", cl, ") for: ", v)

      # Replace 'f' with custom aggregation function, if provided
      if (v %in% names(agg_fun)) f <- agg_fun[[v]]

      # Return expression to be evaluated
      paste0(v, " = ", f, "(", v, ")")
    })

    # Report the aggregation calls to console
    cat(paste(" ", pcalls, collapse = "\n"), "\n")

    # Do the data.table aggregation call
    cat("Aggregating person-level fusion variables to household-level...\n")
    fcall <- paste("list(", paste(pcalls, collapse = ", "), ")")
    d2 <- d2[, eval(parse(text = fcall)), by = did]

    # Merge 'd2' and 'd1'
    # Now we have all fusion variables at household-level
    stopifnot(nrow(d2) == nrow(d1))
    fusion.data <- merge(d2, d1, by = did) %>%
      select(any_of(c(did, "pid", fvars)))

  } else {

    # If no aggregation necessary, simply return 'd1'
    fusion.data <- d1

  }

  # Clean up
  rm(d1, d2)

  #-----

  # Merge donor microdata components: harmonized variables, fusion variables, and location variables
  # Note that the weights are 'integerized' after applying the imputation adjustment factor (weight_adjustment)
  dout <- x$harmonized[[donor]] %>%
    select(-any_of(drop)) %>%  # Remove harmonized predictors to be dropped
    left_join(fusion.data, by = did) %>%
    left_join(x$location[[donor]], by = did[1]) %>%
    mutate(weight = weight * weight_adjustment,
           weight = integerize(weight, mincor = 0.999)) %>%
    select(-weight_adjustment)

  rm(fusion.data)
  gc()

  #-----

  # Merge recipient microdata components: harmonized variables and location variables
  rout <- x$harmonized[[recipient]] %>%
    select(-any_of(drop)) %>%   # Remove harmonized predictors to be dropped
    left_join(x$location[[recipient]], by = c(rid, gtarget))

  rm(x)
  gc()

  #-----

  # Function to load requested spatial predictor variables, based on specified 'spatial.datasets' and 'window'
  loadSpatial <- function(datasets, survey) {

    # Survey vintage (year); returns midpoint in case of range (e.g. "2015-2016" returns 2015.5)
    svintage <- sub("-", ":", str_extract(survey, "\\d{4}.*"), fixed = TRUE)
    svintage <- median(eval(parse(text = svintage)))

    # Vintage of each row in 'geo' fst object assumed to be in outer environment
    gvintage <- geo[["vintage"]]

    # Preferred order of "valid" vintage values, based on 'window'
    #valid <- (svintage - window):(svintage + window)
    valid <- suppressWarnings(na.omit(unique(as.integer(levels(gvintage)))))  # Remove 'window' argument; uses closest available vintage
    delta <- valid - svintage
    delta <- abs(delta + ifelse(delta < 0, 0.1, 0))  # Prioritizes older data in case of tie in closeness to 'svintage'
    valid <- c('always', valid[order(delta)])

    # Loop through 'valid' vintages and return preferred available data
    j <- setdiff(gvars[gsets %in% datasets], c(gtarget, "vintage"))
    out <- list(distinct(geo[gtarget]))
    for (i in valid) {
      ind <- gvintage == i
      if (any(ind)) {
        g <- geo[ind, c(gtarget, j)]
        g <- g[, colSums(!is.na(g)) > 0]
        if (ncol(g) > 2) {
          j <- setdiff(j, names(g))
          out[[i]] <- g
        }
      }
      if (length(j) == 0) break()
    }

    # Merge all vintage subsets on state and PUMA
    out <- Reduce(function(...) merge(..., by = gtarget, all.x = TRUE), compact(out))

    # Order columns sensibly
    out <- out[c(gtarget, intersect(gvars, names(out)))]

    # Remove columns with no variation
    keep <- !sapply(out, novary)
    out <- out[, keep]

    return(out)

  }

  #-----

  # 9/11/24: Force use of spatial predictors from time period of the DONOR. This ensures that the spatial predictors consist of a single vintage.
  # Useful in case where there is time discrepancy between donor and recipients. We want to force the spatial predictors to be those closest to the donor vintage.

  # Merge requested spatial predictors
  if (spatial.datasets[1] != "none") {

    #gsets <- if (spatial.datasets[1] == "all") gsets else spatial.datasets

    # Donor spatial predictors
    dgeo <- loadSpatial(gsets, donor)

    # Impute missing values (too slow)
    #test <- fusionModel::impute(dgeo, ignore = "puma10")

    # Recipient spatial predictors
    #rgeo <- loadSpatial(gsets, recipient)

    # Restrict to common spatial predictors
    # mvars <- c('state', 'puma10')
    # svars <- setdiff(intersect(names(dgeo), names(rgeo)), mvars)
    # dgeo <- dgeo[c(mvars, svars)]
    # rgeo <- rgeo[c(mvars, svars)]

    #-----

    # Perform PCA on numeric spatial predictors, if requested

    if (!is.null(pca)) {

      cat("Performing principal components analysis...\n")

      # Donor PCA results

      # Numeric spatial variables (eligible for PCA)
      ind <- sapply(dgeo, is.numeric)

      # Impute NA's in donor
      dgeo.num <- dgeo[ind]
      na.cols <- names(which(sapply(dgeo.num, anyNA)))
      dgeo.num[na.cols] <- map(dgeo.num[na.cols], ~ replace(.x, list = is.na(.x), values = median(.x, na.rm = TRUE)))

      # Fit PCA to donor data
      pca.fit <- prcomp(x = dgeo.num, retx = TRUE, center = TRUE, scale. = TRUE, rank. = pca[1])

      # Variance explained
      varex <- cumsum(pca.fit$sdev ^ 2 / sum(pca.fit$sdev ^ 2))[1:ncol(pca.fit$x)]

      # Index of final component to retain
      k <- min(which(varex >= pca[2])[1], length(varex), na.rm = TRUE)
      #plot(varex)
      #abline(v = k, h = pca[2])

      # PCA output with updated column names
      dpca <- pca.fit$x[, 1:k]
      colnames(dpca) <- paste0("pca..", colnames(dpca))

      #---

      # # Recipient PCA results
      #
      # # Impute NA's in recipient
      # rgeo.num <- rgeo[ind]
      # na.cols <- names(which(sapply(rgeo.num, anyNA)))
      # rgeo.num[na.cols] <- map(rgeo.num[na.cols], ~ replace(.x, list = is.na(.x), values = median(.x, na.rm = TRUE)))
      #
      # # PCA output with updated column names
      # rpca <- predict(pca.fit, newdata = rgeo.num)[, 1:k]
      # colnames(rpca) <- colnames(dpca)

      #---

      # Update 'dgeo' and 'rgeo' with PCA results
      dgeo <- cbind(dgeo[!ind], dpca)
      #rgeo <- cbind(rgeo[!ind], rpca)

      # Update 'svars' object
      svars <- setdiff(names(dgeo), mvars)

      # Clean up
      #rm(dgeo.num, rgeo.num, dpca, rpca, pca.fit)
      rm(dgeo.num, dpca, pca.fit)
      gc()

    }

    #-----

    # Apply integer scaling to double variables in 'dgeo' and 'rgeo'
    # The scaling median and mad are derived from 'rgeo' and applied to both inputs
    # cat("Applying integer scaling to spatial predictor variables...\n")
    # ind <- sapply(dgeo, is.double) & !names(dgeo) %in% mvars
    # temp <- scale2integer(x = rgeo[ind], y = dgeo[ind], precision = 2)
    # rgeo[ind] <- temp$x
    # dgeo[ind] <- temp$y
    # rm(temp)

    #-----

    # No longer necessary to merge, because storing spatial predictors separately

    # Merge spatial data at PUMA level to DONOR
    # cat("Merging spatial predictor variables to the donor...\n")
    # dout <- left_join(dout, dgeo, by = mvars)
    #
    # # Merge spatial data at PUMA level to RECIPIENT
    # cat("Merging spatial predictor variables to the recipient...\n")
    # #rout <- left_join(rout, rgeo, by = mvars)
    # rout <- left_join(rout, dgeo, by = mvars)  # Merge the donor spatial predictors

  } else {

    svars <- NULL

  }

  #-----

  cat("Converting numeric predictor variables to ranks, when possible...\n")

  # Apply integer scaling to numeric spatial predictors in 'dgeo'
  # Since only the ranks matter for GBM prediction, this reduces memory required
  dgeo <- dgeo %>%
    mutate_if(is.double, data.table::frank, na.last = "keep", ties.method = "dense")

  # If a numeric harmonized predictor variable has more than 100 unique values,
  #   treat it as continuous, scale it, and convert the scaled values to integer ranks.
  # This uses robust Z-scores computed on each sample (donor and recipient) to assign the ranks, which makes the predictors less susceptible to distributional shift between the two samples
  # For example, questions measuring similar concepts (e.g. income) may have different magnitudes, but this scaling ensures that the medians and dispersion around them are treated as equal in the two samples
  for (v in hvars) {
    x <- dout[[v]]
    if (is.numeric(x) & uniqueN(x) > 100) {
      zvals <- lapply(c('dout', 'rout'), function(i) {
        w <- get(i)$weight
        i <- x != 0
        xmed <- matrixStats::weightedMedian(x[i], w[i], na.rm = TRUE)
        xmad <- matrixStats::weightedMad(x[i], w[i], na.rm = TRUE)
        z <- (x - xmed) / xmad
        z <- signif(z, 3)
        return(z)
      })
      # This replaces original values with equivalent of a dense rank
      u <- sort(unique(unlist(zvals)))
      dout[[v]] <- match(zvals[[1]], u)
      rout[[v]] <- match(zvals[[2]], u)
    }
  }

  #-----

  cat("Assembling output data frames...\n")

  # Reorder output columns for nicer viewing
  # Row-order the results by the identifier variable(s)

  dout <- dout %>%
    select(any_of(c(did, "weight", gtarget, fvars, hvars, lvars))) %>%
    #select(any_of(c(did, "weight", fvars, hvars, lvars, svars, rvars))) %>%
    #mutate_at(hvars, ~ convert2scaled(x = ., w = weight, min.unique = 100, precision = 3)) %>%
    arrange_at(did)

  rout <- rout %>%
    select(any_of(c(rid, "weight", gtarget, hvars, lvars))) %>%
    #select(any_of(c(rid, "weight", hvars, lvars, svars))) %>%
    #mutate_at(hvars, ~ convert2scaled(x = ., w = weight, min.unique = 100, precision = 3)) %>%
    arrange_at(rid)

  #-----

  # SAFETY CHECKS: Confirm validity of predictor variables
  # These are identical to the safety checks performed by fusionModel::fuse()
  cat("Performing consistency checks...\n")

  xvars <- setdiff(intersect(names(dout), names(rout)), "weight")
  dclass <- lapply(dout[xvars], class)
  rclass <- lapply(rout[xvars], class)
  miss <- !map2_lgl(dclass, rclass, sameClass)
  if (any(miss)) stop("Incompatible data type/class for the following predictor variables:\n", paste(names(miss)[miss], collapse = ", "))

  # Check for appropriate levels of factor predictor variables
  fxvars <- names(select_if(dout[xvars], is.factor))
  dlevels <- lapply(dout[fxvars], levels)
  rlevels <- lapply(rout[fxvars], levels)
  miss <- !map2_lgl(dlevels, rlevels, identical)
  if (any(miss)) stop("Incompatible levels for the following factor predictor variables\n", paste(names(miss)[miss], collapse = ", "))

  #----

  # Convert 'dout', 'rout', and 'dgeo' to data.table keyed on state and PUMA
  dout <- data.table(dout, key = gtarget)
  rout <- data.table(rout, key = gtarget)
  dgeo <- data.table(dgeo, key = gtarget)

  # Return as list
  #result <- setNames(list(dout, rout), c(donor, recipient))
  result <- setNames(list(dout, rout, dgeo), c(donor, recipient, "spatial"))

  # Set attributes for the returned objects
  setattr(result, "fusion.vars", fvars)
  setattr(result, "harmonized.vars", hvars)
  setattr(result, "location.vars", lvars)
  setattr(result, "spatial.vars", setdiff(names(dgeo), gtarget))
  setattr(result, "merge.vars", gtarget)
  setattr(result, "replicate.vars", rvars)
  setattr(result, "donor.id", did)
  setattr(result, "recipient.id", rid)

  return(result)

}

# Custom aggregation functions made available for use in assemble()
# This can be used in the 'agg_fun' input list
# Returns first value in 'x'
ref <- function(x, na.rm = TRUE) x[1]
# Returns modal value of 'x'
mode <- function(x, na.rm = TRUE) data.table(x = if (na.rm) na.omit(x) else x)[, .N, by = x][order(N, decreasing = TRUE)]$x[1]

