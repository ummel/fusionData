#' Prepare microdata for survey fusion
#'
#' @description
#' Prepares data inputs to pass to \code{\link[fusionModel]{train}} and \code{\link[fusionModel]{fuse}} to perform survey fusion. Harmonizes common variables for the specified donor and recipient surveys, merges requested fusion variables to donor, imputes PUMA for donor records, and merges requested spatial variables to both donor and recipient.
#'
#' @param donor Character. Donor survey identifier (e.g. `"RECS_2015"`).
#' @param recipient Character. Recipient (ACS) survey identifier (e.g. `"ACS_2019"`).
#' @param respondent Character. Desired respondent level of microdata. Either `"household"` or `"person"`.
#' @param ... Optional arguments passed to \code{\link[dplyr]{select}} for selecting (or excluding) particular donor variables. Passed to \link{completeDonor}.
#' @param implicates Integer. Number of PUMA implicates to return for the donor microdata.
#' @param spatial.datasets Character. Vector of requested spatial datasets to merge (e.g. `"EPA-SLD"`) or either of two special values: `"all"` (default) or `"none"`.
#' @param window Integer. Size of allowable temporal window, in years, when merging spatial variables. `window = 0` (default) means that a spatial variable is only included if it has the same vintage as the survey. See Details.
#' @param pca Numeric. Controls whether/how PCA is used to reduce dimensionality of spatial variables. Default (NULL) is no PCA. If non-NULL, should be a numeric vector of length two; e.g. \code{pca = c(50, 0.95)}. First number is the maximum number of components to return; second number is target proportion of variance explained. See Details.
#' @param replicates Logical. Should replicate observation weights be included, if available? Defaults to FALSE. Passed to \link{completeDonor}.
#'
#' @details Spatial variables are included if the associated vintage is within +/- `window` years of the survey vintage. In cases where the spatial variable has multiple vintages equidistant from the survey vintage, the older vintage is selected. Variables with `vintage = "always"` are, of course, always included.
#'
#' @details PCA is restricted to numeric spatial variables and is computed using \code{\link[stats]{prcomp}}. The first number in \code{pca} is passed to argument \code{rank.}. The returned number of principal components is the lesser of \code{pca[1]} or the number of components that explain at least \code{pca[2]} proportion of the variance. For example, \code{pca = c(50, 0.95)} will select the fewest number of components that explain 95% of the variance, up to 50 components maximum. NA's in numeric spatial variables are replaced with median value prior to computing the principal components.
#'
#' @return A list of length two containing donor and recipient microdata.
#'
#' @examples
#' data <- prepare(donor = "RECS_2015",
#'                 recipient = "ACS_2019",
#'                 respondent = "household",
#'                 cooltype, agecenac, kwhcol,
#'                 window = 3)
#'
#' @export

#-----

#source("R/utils.R")
#
# Example usage
# test <- prepare(donor = "RECS_2015",
#                 recipient = "ACS_2019",
#                 respondent = "household",
#                 cooltype, agecenac, kwhcol,
#                 implicates = 5,
#                 window = 3,
#                 pca = c(100, 0.95))

#-----

prepare <- function(donor,
                    recipient,
                    respondent,
                    ...,
                    implicates = 1,
                    spatial.datasets = "all",
                    window = 0,
                    pca = NULL,
                    replicates = FALSE) {

  stopifnot(basename(getwd()) == "fusionData")

  #-----

  geo <- fst::fst("geo-processed/geo_predictors.fst")
  gvars <- setdiff(names(geo), c('state', 'puma10', 'vintage'))
  gsets <- map_chr(strsplit(gvars, "..", fixed = TRUE), 1L)

  # Validate arguments
  stopifnot({
    respondent %in% c("household", "person")
    implicates >= 1 & implicates %% 1 == 0
    spatial.datasets[1] %in% c("all", "none") | all(spatial.datasets %in% unique(gsets))
    window >= 0 & window %% 1 == 0
    is.null(pca) | (is.numeric(pca) & length(pca) == 2)
  })

  #-----

  # This could be one-time call stored remotely? Can store as .rds
  data <- harmonize(harmony.file = paste0(donor, "__", recipient, ".R"), respondent = respondent)

  # Names of harmonized variables
  hvars <- setdiff(intersect(names(data[[1]]), names(data[[2]])), "pid")

  # Respondent identifier variables in donor and recipient
  did <- setdiff(names(data[[1]]), hvars)
  rid <- setdiff(names(data[[2]]), c(hvars, "state", "puma10"))

  #-----

  # Impute PUMA for the donor
  dpuma <- imputePUMA(survey = donor, m = implicates)

  #-----

  # Add donor variables to be fused
  # Includes harmonized predictors
  cat("Loading donor fusion variables...\n")
  dout <- completeDonor(data = data[[1]], ..., replicates = replicates)
  fusevars <- attr(dout, "fusion.vars")  # fusion variables
  repvars <- attr(dout, "replicate.vars")  # replicate weight variables
  dout <- select(dout, -any_of(attr(dpuma, "geo.vars")))
  dout <- dpuma %>%
    left_join(dout, by = names(dpuma)[1]) %>%
    mutate(weight = weight * weight_adjustment) %>%
    select(-weight_adjustment)

  #-----

  # Recipient output
  rout <- data[[2]]

  #-----

  # Function to load requested spatial predictor variables, based on specified 'spatial.datasets' and 'window'

  loadSpatial <- function(datasets, survey) {

    # Survey vintage (year); returns midpoint in case of range (e.g. "2015-2016" returns 2015.5)
    svintage <- sub("-", ":", str_extract(survey, "\\d{4}.*"), fixed = TRUE)
    svintage <- median(eval(parse(text = svintage)))

    strsplit(survey, "_", fixed = TRUE)

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

    if (spatial.datasets != "all") gsets <- intersect(gsets, spatial.datasets)

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
    cat("Merging donor spatial variables...\n")
    dout <- left_join(dout, dgeo, by = mvars)

    # Merge spatial data at PUMA level to RECIPIENT
    cat("Merging recipient spatial variables...\n")
    rout <- left_join(rout, rgeo, by = mvars)

  } else {

    svars <- NULL

  }

  #-----

  # FIX -- dropping vars here! update svars!

  # Reorder variables for nicer viewing
  # Also coerces from data.table to data.frame (NECESSARY?)

  dout <- dout %>%
    select(any_of(c(did, "weight", fusevars, hvars, svars, repvars))) %>%
    as.data.frame()

  rout <- rout %>%
    select(any_of(c(rid, hvars, svars))) %>%
    as.data.frame()

  #-----

  # SAFETY CHECKS: Confirm validity of predictor variables
  cat("Performing validation checks...\n")
  xvars <- intersect(names(dout), names(rout))

  dclass <- lapply(dout[xvars], class)
  rclass <- lapply(rout[xvars], class)
  miss <- !map2_lgl(dclass, rclass, identical)
  if (any(miss)) stop("Incompatible data type for the following predictor variables:\n", paste(names(miss)[miss], collapse = ", "))

  # Check for appropriate levels of factor predictor variables
  fvars <- names(select_if(dout[xvars], is.factor))
  dlevels <- lapply(dout[fvars], levels)
  rlevels <- lapply(rout[fvars], levels)
  miss <- !map2_lgl(dlevels, rlevels, identical)
  if (any(miss)) stop("Incompatible levels for the following predictor variables\n", paste(names(miss)[miss], collapse = ", "))

  #-----

  # Assign "fusion.vars" "harmonized.vars", and "spatial.vars" attributes to 'dout'
  attr(dout, "fusion.vars") <- fusevars
  attr(dout, "harmonized.vars") <- hvars
  attr(dout, "spatial.vars") <- svars
  attr(dout, "replicate.vars") <- repvars

  # Same for recipient, but excluding "fusion.vars"
  attr(rout, "harmonized.vars") <- hvars
  attr(rout, "spatial.vars") <- svars

  # Return as list...
  result <- setNames(list(dout, rout), c(donor, recipient))

  return(result)

}
