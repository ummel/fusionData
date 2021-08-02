#' Assemble data used for survey fusion
#'
#' @description
#' Assembles data inputs to pass to \code{\link[fusionModel]{train}} and \code{\link[fusionModel]{fuse}} to perform survey fusion. Adds fusion, replicate weight, and/or spatial variables and checks that donor and recipient output data frames are consistent.
#'
#' @param x List object produced by \code{\link{prepare}}.
#' @param fusion.variables Character. Names of donor variables to be included in output as fusion candidates. If NULL (default), an attempt is made to return all donor variables not used in harmonization process.
#' @param spatial.datasets Character. Vector of requested spatial datasets to merge (e.g. `"EPA-SLD"`) or either of two special values: `"all"` (default) or `"none"`.
#' @param window Integer. Size of allowable temporal window, in years, when merging spatial variables. `window = 0` (default) means that a spatial variable is only included if it has the same vintage as the survey. See Details.
#' @param pca Numeric. Controls whether/how PCA is used to reduce dimensionality of spatial variables. Default (NULL) is no PCA. If non-NULL, should be a numeric vector of length two; e.g. \code{pca = c(50, 0.95)}. First number is the maximum number of components to return; second number is target proportion of variance explained. See Details.
#' @param replicates Logical. Should replicate observation weights be included, if available? Defaults to FALSE. Passed to \link{completeDonor}.
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

# x: list produced by prepare()
# fusion.variables = NULL
# replicates = FALSE
# spatial.datasets = "all"
# window = 3
# pca = c(100, 0.95)

#-----

assemble <- function(x,
                     fusion.variables = NULL,
                     spatial.datasets = "all",
                     window = 2,
                     pca = NULL,
                     replicates = FALSE) {

  # Names of the donor and recipient surveys
  donor <- names(x$harmonized)[1]
  recipient <- names(x$harmonized)[2]

  # Respondent identifier variables in donor and recipient (possibly including 'pid')
  did <- attr(x$harmonized[[1]], "identifier")
  rid <- attr(x$harmonized[[2]], "identifier")

  # Respondent type
  respondent <- ifelse("pid" %in% did, "person", "household")

  # Names of harmonized variables
  hvars <- attr(x$harmonized, "harmonized.vars")

  # Names of the location variables ("loc..*")
  lvars <- attr(x$location, "location.vars")

  #-----

  # # Identify spatial variables and datasets available in 'geo_predictors.fst'
  geo <- fst::fst("geo-processed/geo_predictors.fst")
  gvars <- setdiff(names(geo), c('state', 'puma10', 'vintage'))
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

  # Determine which donor fusion variables to load from disk
  cat("Identifying donor fusion variables...\n")

  # Load household data (NULL if unavailable or unnecessary)
  fpath <- list.files(path = "survey-processed", pattern = paste(donor, ifelse(respondent == "household", "H", "P"), "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
  d <- fst::fst(fpath)

  # Names of replicate weight variables
  rvars <- grep("^rep_\\d+$", names(d), value = TRUE)

  # Identify the fusion variables
  disallowed <- unique(c(did, rvars, "weight", "state", "puma10", attr(x$location, "intersection.vars")))  # Variables that are not feasible fusion candidates
  employed <- attr(x$harmonized[[donor]], "employed.vars")
  fvars <- if (is.null(fusion.variables)) {
    setdiff(names(d), c(disallowed, employed))  # Exclude variables known to be used for harmonization
  } else {
    invalid1 <- setdiff(fusion.variables, names(d))
    if (length(invalid1) > 0) cat("Some requested fusion variables are not in the donor microdata:\n", paste(invalid1, collapse = ", "), "\n")
    invalid2 <- intersect(fusion.variables, disallowed)
    if (length(invalid2) > 0) cat("Some requested fusion variables are not feasible candidates:\n", paste(invalid2, collapse = ", "), "\n")
    invalid3 <- intersect(fusion.variables, employed)
    if (length(invalid3) > 0) cat("Some requested fusion variables were used to create harmonies:\n", paste(invalid3, collapse = ", "), "\n")
    setdiff(fusion.variables, c(invalid1, invalid2, invalid3))
  }

  # Report which variables are being added ass fusion variables
  fvars <- sort(fvars)
  cat("Adding the following fusion variables:\n", paste(fvars, collapse = ", "), "\n")

  # Load necessary donor variables from disk
  rvars <- if (replicates) rvars else NULL
  keep <- c(did, fvars, rvars)
  fusion.data <- d[keep]

  #-----

  # Merge donor microdata components: harmonized variables, fusion variables, and location variables
  dout <- x$harmonized[[donor]] %>%
    left_join(fusion.data, by = did) %>%
    left_join(x$location[[donor]], by = did[1]) %>%
    mutate(weight = weight * weight_adjustment) %>%
    select(-weight_adjustment)

  rm(fusion.data)
  gc()

  #-----

  # Merge recipient microdata components: harmonized variables and location variables
  rout <- x$harmonized[[recipient]] %>%
    left_join(x$location[[recipient]], by = rid[1])

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
    valid <- (svintage - window):(svintage + window)
    delta <- valid - svintage
    delta <- abs(delta + ifelse(delta < 0, 0.1, 0))
    valid <- c('always', valid[order(delta)])

    # Loop through 'valid' vintages and return preferred available data
    j <- setdiff(gvars[gsets %in% datasets], c("state", "puma10", "vintage"))
    out <- list(distinct(geo[c("state", "puma10")]))
    for (i in valid) {
      ind <- gvintage == i
      if (any(ind)) {
        g <- geo[ind, c('state', 'puma10', j)]
        g <- g[, colSums(!is.na(g)) > 0]
        if (ncol(g) > 2) {
          j <- setdiff(j, names(g))
          out[[i]] <- g
        }
      }
      if (length(j) == 0) break()
    }

    # Merge all vintage subsets on 'state' and 'puma10'
    out <- Reduce(function(...) merge(..., by = c('state', 'puma10'), all.x = TRUE), compact(out))

    # Order columns sensibly
    out <- out[c('state', 'puma10', intersect(gvars, names(out)))]

    # Remove columns with no variation
    keep <- !sapply(out, novary)
    out <- out[, keep]

    return(out)

  }

  #-----

  # Merge requested spatial predictors
  if (spatial.datasets != "none") {

    gsets <- if (spatial.datasets == "all") gsets else spatial.datasets

    # Donor spatial predictors
    dgeo <- loadSpatial(gsets, donor)

    # Donor spatial predictors
    rgeo <- loadSpatial(gsets, recipient)

    # Restrict to common spatial predictors
    mvars <- c('state', 'puma10')
    svars <- setdiff(intersect(names(dgeo), names(rgeo)), mvars)
    dgeo <- dgeo[c(mvars, svars)]
    rgeo <- rgeo[c(mvars, svars)]

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
      dgeo.num[na.cols] <- map(dgeo.num[na.cols], ~ replace_na(.x, replace = median(.x, na.rm = TRUE)))

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

      # Recipient PCA results

      # Impute NA's in recipient
      rgeo.num <- rgeo[ind]
      na.cols <- names(which(sapply(rgeo.num, anyNA)))
      rgeo.num[na.cols] <- map(rgeo.num[na.cols], ~ replace_na(.x, replace = median(.x, na.rm = TRUE)))

      # PCA output with updated column names
      rpca <- predict(pca.fit, newdata = rgeo.num)[, 1:k]
      colnames(rpca) <- colnames(dpca)

      #---

      # Update 'dgeo' and rgeo' with PCA results
      dgeo <- cbind(dgeo[!ind], dpca)
      rgeo <- cbind(rgeo[!ind], rpca)

      # Update 'svars' object
      svars <- setdiff(names(dgeo), mvars)

      # Clean up
      rm(dgeo.num, rgeo.num, dpca, rpca, pca.fit)
      gc()

    }

    #-----

    # Merge spatial data at PUMA level to DONOR
    cat("Merging donor spatial predictor variables...\n")
    dout <- left_join(dout, dgeo, by = mvars)

    # Merge spatial data at PUMA level to RECIPIENT
    cat("Merging recipient spatial predictor variables...\n")
    rout <- left_join(rout, rgeo, by = mvars)

  } else {

    svars <- NULL

  }

  #-----

  # Reorder output variables for nicer viewing
  # Note that numeric 'hvars' are converted to percentile in certain cases; see convertPercentile() in R/utils.R

  cat("Assembling output data frames...\n")

  dout <- dout %>%
    select(any_of(c(did, "weight", fvars, hvars, lvars, svars, rvars))) %>%
    mutate_at(hvars, ~ convertPercentile(x = ., w = weight, min.unique = 100, min.zero = 0.05))

  rout <- rout %>%
    select(any_of(c(rid, "weight", hvars, lvars, svars))) %>%
    mutate_at(hvars, ~ convertPercentile(x = ., w = weight, min.unique = 100, min.zero = 0.05))

  #-----

  # SAFETY CHECKS: Confirm validity of predictor variables
  # These are identical to the safety checks performed by fusionModel::fuse()
  cat("Performing validation checks...\n")

  xvars <- setdiff(intersect(names(dout), names(rout)), "weight")
  dclass <- lapply(dout[xvars], class)
  rclass <- lapply(rout[xvars], class)
  miss <- !map2_lgl(dclass, rclass, sameClass)
  if (any(miss)) stop("Incompatible data type for the following predictor variables:\n", paste(names(miss)[miss], collapse = ", "))

  # Check for appropriate levels of factor predictor variables
  fxvars <- names(select_if(dout[xvars], is.factor))
  dlevels <- lapply(dout[fxvars], levels)
  rlevels <- lapply(rout[fxvars], levels)
  miss <- !map2_lgl(dlevels, rlevels, identical)
  if (any(miss)) stop("Incompatible levels for the following predictor variables\n", paste(names(miss)[miss], collapse = ", "))

  #-----

  # Set attributes for the returned objects

  # Assign "fusion.vars" "harmonized.vars", and "spatial.vars" attributes to 'dout'
  # setattr(dout, "fusion.vars", fvars)
  # setattr(dout, "harmonized.vars", hvars)
  # setattr(dout, "location.vars", lvars)
  # setattr(dout, "spatial.vars", svars)
  # setattr(dout, "replicate.vars", rvars)
  #
  # # Same for recipient, but excluding "fusion.vars"
  # setattr(rout, "harmonized.vars", hvars)
  # setattr(rout, "location.vars", lvars)
  # setattr(rout, "spatial.vars", svars)

  # Return as list
  result <- setNames(list(dout, rout), c(donor, recipient))

  setattr(result, "fusion.vars", fvars)
  setattr(result, "harmonized.vars", hvars)
  setattr(result, "location.vars", lvars)
  setattr(result, "spatial.vars", svars)
  setattr(result, "replicate.vars", rvars)

  return(result)

}
