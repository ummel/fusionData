#' Impute and Assign Spatial Locations for Microdata
#'
#' @description
#' `imputeLocation()` is a core architectural helper used within \code{\link{fusionInput}}.
#' It reconciles geographic differences between donor and recipient microdata datasets by
#' imputing Public Use Microdata Areas (PUMAs) for donor survey records and
#' assigning common geographic intersection variables to recipient records.
#'
#' @details
#' Microdata sources often report spatial resolution at different levels of granularity.
#' For example, donor surveys (like AHS or RECS) may report coarse geographies (e.g., CBSA or
#' Census Region), whereas recipient datasets (like the ACS) report detailed PUMAs.
#'
#' `imputeLocation()` bridges this gap in two steps:
#' \enumerate{
#'   \item \strong{Donor Imputation}: Uses Gower's distance via \code{\link[gower]{gower_topn}}
#'   to calculate similarity across common household-level variables between donor and recipient
#'   households within matching geographic intersections. It then samples and imputes a target
#'   PUMA for each donor observation.
#'   \item \strong{Recipient Assignment}: Maps common geographic intersection variables back to
#'   recipient observations by sampling from geographic concordance tables (\code{geo_concordance.fst})
#'   proportional to household density (\code{puma_share}).
#' }
#'
#' Retained spatial variables are renamed with a \code{loc..} prefix in the returned list to
#' denote confirmed or assigned spatial indicators.
#'
#' @param harmonized List. The output object from a call to \code{\link{harmonize}}, containing
#'   the paired donor and recipient microdata datasets as its first and second elements.
#' @param ncores Integer. Number of compute cores passed directly to \code{\link[gower]{gower_topn}}
#'   for parallel distance calculation across observations.
#'
#' @return A named \code{list} of two \code{data.table} objects mirroring the structure of
#'   \code{harmonized}:
#'   \item{donor}{Donor microdata containing the original household ID, target PUMA variables,
#'   and imputed location variables prefixed with \code{loc..}.}
#'   \item{recipient}{Recipient microdata containing the original household ID, target PUMA
#'   variables, and assigned location variables prefixed with \code{loc..}.}
#'
#' The returned list carries attributes:
#' \itemize{
#'   \item \code{location.vars}: Character vector of assigned location column names (prefixed with \code{loc..}).
#'   \item \code{intersection.vars}: Character vector of spatial intersection variables shared across datasets.
#' }
#'
#' @keywords internal
#' @seealso \code{\link{harmonize}}, \code{\link{fusionInput}}
#' @noMd

#-----

imputeLocation <- function(harmonized, ncores) {

  # Variables in geo_concordance.fst defining the "target" geography (i.e. uniquely-identified PUMA's)
  # Automatically detected from 'harmonized' object produced by harmonize()
  gtarget <- attr(harmonized[[2]], "geo.vars")

  # The PUMA-related weight variable in 'glink' (i.e. housing unit count)
  gw <- "puma_weight"

  # Household ID variables
  did <- did0 <- attr(harmonized[[1]], "identifier")[1]
  rid <- rid0 <- attr(harmonized[[2]], "identifier")[1]

  # Ensure the household ID variables are uniquely-named (bit of a kluge, really)
  did <- paste0("donor_", did)
  rid <- paste0("recipient_", rid)

  #-----

  # Soft load the geo_concordance.fst file
  # Select based on vintage of 'gtarget' PUMA variable (puma10, puma20, etc.)
  glink <- fst::fst(paste0("geo-processed/concordance/geo_concordance_20", substring(gtarget[startsWith(gtarget, "puma")], 5), ".fst"))

  # Soft load the specified donor survey processed .fst file
  temp <- list.files(path = "survey-processed", pattern = paste0("^", names(harmonized)[1], "_._processed.fst"), recursive = TRUE, full.names = TRUE)
  temp <- temp[1L]  # This retains the household file in event that 'survey' has both H and P data (only H needed to sample PUMAs)
  D <- fst::fst(temp)

  #-----

  # TESTING: Insert AHS-specific processing of CBSA codes
  # The AHS CBSA code variable is "omb13cbsa_code" in AHS 2023
  # Inserting here, because the CBSA codes in the data vary between donor years, so knowing which code are in the donor (D) is necessary
  if (startsWith(names(harmonized)[1], "AHS")) {
    if ("cbsa10" %in% names(glink)) {
      glink <- glink[] %>%
        mutate(
          cbsa10 = ifelse(cbsa10 == "31100", "31080", cbsa10), # Manual kludge for the Los Angeles-Long Beach-Anaheim, CA Metropolitan Statistical Area; it was code 31100 under the 2003 definitions and was reassigned to 31080 in later (2013) CBSA updates.
          omb13cbsa_code = ifelse(cbsa10 == "None", 99999, cbsa10),
          omb13cbsa_code = ifelse(omb13cbsa_code %in% D$omb13cbsa_code, omb13cbsa_code, 99998),
          omb13cbsa_code = as.integer(omb13cbsa_code)
        )
    }
    if ("cbsa20" %in% names(glink)) {
      glink <- glink[] %>%
        mutate(
          cbsa20 = ifelse(cbsa20 == "31100", "31080", cbsa20), # Manual kludge for the Los Angeles-Long Beach-Anaheim, CA Metropolitan Statistical Area; it was code 31100 under the 2003 definitions and was reassigned to 31080 in later (2013) CBSA updates.
          omb13cbsa_code = ifelse(cbsa20 == "None", 99999, cbsa20),
          omb13cbsa_code = ifelse(omb13cbsa_code %in% D$omb13cbsa_code, omb13cbsa_code, 99998),
          omb13cbsa_code = as.integer(omb13cbsa_code)
        )
    }
  }

  #-----

  # Identify geographic intersection variables in 'data'
  gdonor <- intersect(names(glink), names(D))

  # Report geographic intersection variables
  cli::cli_alert_info("Identified the following geographic intersection variables: {paste(gdonor, collapse = ', ')}")

  #-----

  # Read the necessary 'glink' variables from disk and set to keyed data.table
  gv <- unique(c(gtarget, gdonor))
  glink <- glink[c(gw, gv)] %>%
    #mutate_if(is.character, factor) %>%  # Converts any characters to factor (NOTE: perform elsewhere???)
    mutate_at(gv, as.factor) %>%
    data.table(key = gv) %>%
    setnames(c("W", gv))  # Rename the 'gw' variable to "W" for ease of use in data.table operations

  # Aggregate weight to reduce number of observations
  glink <- glink[, .(W = sum(W)), by = gv]

  # Retain only complete cases (drops any rows with NA values; for example, if Puerto Rico is included in glink)
  glink <- glink[complete.cases(glink[, ..gv])]

  #---

  # Load the necessary geographic donor variables and convert to keyed data.table
  D <- D[c(did0, gdonor)] %>%
    setnames(old = did0, new = did) %>%
    data.table(key = gdonor)

  # Ensure the factor levels are consistent between 'glink' and 'D' (possible that latter could be missing some levels)
  # Will stop with error if there are levels in 'D' not found in 'glink'
  # TO DO: Make this error more information for troubleshooting
  for (v in gdonor) {
    if (!all(unique(D[[v]]) %in% levels(glink[[v]]))) {
      stop("Inconsistent levels for geographic intersection variable: ", v)
    }
    set(D, j = v, value = factor(D[[v]], levels = levels(glink[[v]]), ordered = is.ordered(glink[[v]])))
  }

  # # Which geographic intersection variables should be returned as "loc.." variables in output results?
  # # This is restricted to 'gdonor' variables with no missing values in the donor microdata
  # gkeep <- names(which(!sapply(D[, ..gdonor], anyNA)))
  # gkeep <- setdiff(gkeep, gtarget)  # Excludes state and PUMA, if they happen to be in the donor data

  # Restricted to 'gdonor' variables with no missing values AND all national values/levels are present in the donor microdata
  # Excludes from 'gkeep' any variables that are not fully represented in the donor data
  gkeep <- sapply(gdonor, function(v) !anyNA(D[[v]]) & all(glink[[v]] %in% D[[v]]))
  gkeep <- names(gkeep)[gkeep]

  #---

  # Assign integer ID for each geographic intersection defined by the 'gdonor' variables
  D[, id := .GRP, by = gdonor]

  # Report number of unique geographic intersections
  cli::cli_alert_info("Identified {max(D$id)} geographic intersections in the donor...")

  #---

  # In event that there are NA values in 'D', build the processed 'glink' data.table sequentially
  # If there are no NA's, a faster data.table operation can be used

  if (anyNA(D)) {

    f <- function(i) {

      # Subset 'D' for intersection 'i' and remove columns with NA values
      d <- subset(D, id == i)
      d <- d[, .SD, .SDcols = names(which(!sapply(d, anyNA)))]

      if (ncol(d) < ncol(D)) {
        gsum <- unique(c(gdonor, gtarget))
        gtemp <- glink[, .(W = sum(W)), by = gsum]
      } else {
        gtemp <- copy(glink)
      }

      # Add 'id' assignment to 'gtemp' observations
      gtemp[d, id := i.id, on = intersect(gdonor, names(d))]

      # For each PUMA, calculate proportion of households in each intersection (intersection 'i' or NA, in this case)
      # This is used to assign the "naive"/default probability that a household in a given PUMA is also located in intersection 'id'
      gtemp[, puma_share := W / sum(W), by = gtarget]

      # Restrict output to geographic areas within intersection 'i'
      return(subset(gtemp, !is.na(id)))

    }

    # Process all intersections and bind results
    temp <- lapply(1:max(D$id), f) %>%
      rbindlist(fill = TRUE)

    # Identify PUMA's that are in 'glink' but missing from 'temp'
    missing <- glink[!temp, on = gtarget]
    missing$id <- 0L
    missing$puma_share <- 1

    # Append the missing PUMA's to 'temp' and create new 'glink' table
    glink <- rbind(temp, missing)
    rm(temp, missing)

  } else {

    # Add 'id' assignment to 'glink' observations
    glink[D, id := i.id, on = gdonor]

    # For each PUMA, calculate proportion of households in each intersection
    # This is used to assign the "naive"/default probability that a household in a given PUMA is also located in intersection 'id'
    glink[, puma_share := W / sum(W), by = gtarget]

  }

  #-----

  # Check for any 'id' values in 'D' that could not find a match in 'glink'
  miss <- sort(setdiff(unique(D$id), unique(glink$id)))
  if (length(miss) > 0) stop("There are geographic intersections in the donor microdata that could not be matched to the 'geo_concordance' file.\nYou should check that the geographic variables in the two datasets are defined identically.")
  #filter(D, id %in% miss)  # Visual check

  #-----

  # Donor output from harmonize() with 'id' merged
  D <- harmonized[[1]] %>%
    rename_at(1, ~did) %>%
    as.data.table() %>%
    merge(D, by = did)

  # Recipient output from harmonize()
  R <- harmonized[[2]] %>%
    rename_at(1, ~rid) %>%
    as.data.table()

  # Variables to use for distance/similarity calculation via Gower's distance
  X <- setdiff(intersect(names(D), names(R)), c("weight", gdonor, gtarget))

  #-----

  #i <- 5
  #N <- 500
  sampleIntersection <- function(i, N = 500, m = 1, ncores) {

    # Subset 'D' for intersection 'i' and remove columns with NA values
    d <- subset(D, id == i)

    # 'puma_share' is the naive probability that the respondent is in intersection 'id'
    # When default sample weight is multiplied by 'puma_share', we get an estimate of the intersection-specific sample weight
    r <- R %>%
      merge(subset(glink, id == i), by = gtarget, allow.cartesian = TRUE) %>%
      mutate(weight = weight * puma_share) %>% # "naive" likelihood of selecting each household in 'r'
      slice_sample(n = min(N, nrow(.)))  # Takes random sample to reduce number of observations passed to gower_topn()

    # Gower distance for top-N most similar respondents
    G <- gower::gower_topn(x = d[, ..X], y = r[, ..X], n = nrow(r), nthread = ncores)

    # Initial 'weight' of each recipient observation
    # This is the "naive" likelihood of selecting each household in 'r'
    P0 <- G$index
    P0[] <- r$weight[G$index]

    # Adjust any zero distance in 'G' to some arbitrary low value (avoid divide-by-zero errors)
    zero <- G$distance == 0
    if (any(zero)) G$distance[zero] <- 0.5 * min(G$distance[!zero])

    # Adjusted likelihood of selection (naive probability divided by Gower's distance)
    P <- P0 / G$distance

    # Random sampling of 'm' implicates for each respondent in 'd', based on adjusted probability of selection (P)
    S <- sapply(X = 1:ncol(P),
                FUN = function(i) sample(x = G$index[, i], size = m, replace = TRUE, prob = P[, i]),
                simplify = TRUE)

    # Assemble output
    # Includes imputed variables (state, PUMA, recipient ID) and actual (gdonor) geographic variables for each donor household in 'd'
    dkeep <- c(did, gkeep)
    rkeep <- c(rid, gtarget)
    out <- cbind(d[rep(1:nrow(d), times = m), ..dkeep],
                 r[as.vector(t(S)), ..rkeep])

    return(out)

  }

  #---

  cli::cli_alert_info("Imputing PUMA for donor observations...")

  # Troubleshooting
  #for (id in sort(unique(D$id))) sampleIntersection(id)

  D <- pbapply::pblapply(X = sort(unique(D$id)),
                         FUN = sampleIntersection,
                         cl = 1L,  # This defaults to lapply() execution but gives a nice progress bar/timer
                         N = 1000,
                         ncores = ncores) %>%  # 'ncores' are used for the gower_topn() call
    rbindlist()

  # If there are any duplicated column names in 'D'; remove one of them
  # This can occur for geographic variables common to both donor and recipient (e.g. "state")
  # Using 'fromLast = TRUE' removes the first occurrence (i.e. the donor entry), though any duplicate variables *should* be identical
  keep <- !duplicated(names(D), fromLast = TRUE)
  D <- D[, ..keep]

  # Calculate 'weight_adjustment' column
  # When 'D' is merged with microdata, the household "weight" is multiplied by "weight_adjustment" to arrive at correct total sample weight
  # This allow the unique() call below, which reduces the number of row in results (i.e. collapse duplicated entries)
  # Note that the recipient ID is dropped, which is OK if we don't care about adding additional ACS-based predictor variables (might be changed in future)
  # If collapse = FALSE, then the weight_adjustment column is simply 1/m and there is no collapse of duplicate household entries
  data.table::set(D, j = rid, value = NULL)

  # Turned off May 19, 2026 ('collapse' and 'm=1' arguments are deprecated)
  # if (collapse) {
  #   D[, weight_adjustment := .N / m, by = c(did, gtarget)]
  #   D <- unique(D)
  # } else {
  #   D[, weight_adjustment := 1 / m]
  # }

  # Set the column order in 'D'
  D <- setcolorder(D, unique(c(did, gtarget, gkeep)))

  gc()

  #---

  cli::cli_alert_info("Assigning location variables to recipient observations...")

  # Assign 'gkeep' variables to each recipient household
  # There is no guarantee that PUMA's are uniquely identified by the 'gdonor' variables
  # Instead, PUMA boundaries may span intersection boundaries
  # Consequently, the assignment of 'gdonor' variables to the recipient needs to be a random sample (i.e. households in the same PUMA can be assigned to different 'gdonor' intersections)
  # This is accomplished by randomly sampling 'glink' rows within a PUMA to create 'gdonor' assignments for households in 'R'
  # The probability of selection is 'puma_share' in 'glink' (the probability that a HH assigned to that PUMA is in the specified intersection)

  # Restrict 'R' to household ID and PUMA
  # Count the number of households (N) associated with each PUMA
  # Order the rows by PUMA
  R <- R[, .SD, .SDcols = c(rid, gtarget)]
  R[ , N := .N, by = gtarget]
  setorderv(R, cols = gtarget)

  # Add the 'N' count variable to 'glink'; this gives the number of sampled households in each unique PUMA
  # For each PUMA in 'glink', randomly sample 'N' rows, where probability of selection is equal to 'puma_share'
  # The result is a data.table with same number of rows as 'R'
  # Order the rows by PUMA so it is aligned with 'R' and can be cbind'd below
  glink[R, N := i.N, on = gtarget]
  ind <- glink[, .I[sample(.N, size = unique(N), prob = puma_share, replace = TRUE)], by = gtarget]$V1
  glink <- glink[ind]
  setorderv(glink, cols = gtarget)

  # Safety checks to ensure cbind() below is accurate
  stopifnot(nrow(R) == nrow(glink))
  stopifnot(identical2(R$state, glink$state))
  stopifnot(identical2(R$puma10, glink$puma10))

  # Assign 'gkeep' variables for each recipient household
  # This also results in column order similar to 'D'
  rkeep <- unique(c(gtarget, gkeep)) # Ensures that state and PUMA are always retained (i.e. assuming ACS is recipient)
  R <- cbind(R[, ..rid], glink[, ..rkeep])

  #---

  # Rename the 'gkeep' variables to include the "loc.." prefix
  # This identifies them as spatial variables, but "loc" is reserved for variables that are actually known and not imputed
  # This code is more complicates than original setnames(...) in order to safely account for location ('gkeep') variables that are also in 'gtarget'
  # In that case, the 'gtarget' variables are kept as-is, and an identical loc.. version added
  for (v in gkeep) {
    set(D, j = paste0("loc..", v), value = D[[v]])
    set(R, j = paste0("loc..", v), value = R[[v]])
    if (!v %in% gtarget) {
      set(D, j = v, value = NULL)
      set(R, j = v, value = NULL)
    }
  }

  # Set ID variables names back to originals
  setnames(D, old = did, new = did0)
  setnames(R, old = rid, new = rid0)

  # Assemble into final results list
  # Assign attribute indicating the "location" variables ('loc..')
  result <- list(D, R)
  names(result) <- names(harmonized)
  setattr(result, "location.vars", paste0("loc..", gkeep))
  setattr(result, "intersection.vars", gdonor)
  return(result)

}
