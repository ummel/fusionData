#' Impute or assign location for survey records
#'
#' @description
#' Imputes PUMA for donor microdata observations. Assigns available location variables to recipient microdata observations. Used with \link{prepare}. ADD MORE.
#'
#' @param harmonized Output from call to \link{harmonize}.
#' @param m Integer. Number of implicates when imputing PUMA for donor observations.
#'
#' @return A list of length 2.
#'
#' @export

#-----

assignLocation <- function(harmonized, m = 1) {

  # Move elsewhere?
  mc.cores <- max(1L, parallel::detectCores() - 1L)

  # Variables in geolink defining the "target" geography (i.e. uniquely-identified PUMA's)
  gtarget <- c("state", "puma10")

  # The PUMA-related weight variable in 'glink' (i.e. housing unit count)
  gw <- "puma_weight"

  # Household ID variables
  did <- names(harmonized[[1]])[1]
  rid <- names(harmonized[[2]])[1]

  #-----

  # Soft load the geolink.fst file
  glink <- fst::fst("geo-processed/concordance/geo_concordance.fst")

  # Soft load the specified donor survey processed .fst file
  temp <- list.files(path = "survey-processed", pattern = paste0("^", names(harmonized)[1], "_._processed.fst"), recursive = TRUE, full.names = TRUE)
  temp <- temp[1L]  # This retains the household file in event that 'survey' has both H and P data (only H needed to sample PUMAs)
  D <- fst::fst(temp)  # TEMP TURN OFF

  # Soft load the specified donor survey processed .fst file
  temp <- list.files(path = "survey-processed", pattern = paste0("^", names(harmonized)[2], "_._processed.fst"), recursive = TRUE, full.names = TRUE)
  temp <- temp[1L]  # This retains the household file in event that 'survey' has both H and P data (only H needed to sample PUMAs)
  R <- fst::fst(temp)  # TEMP TURN OFF

  #-----

  # Identify geographic intersection variables in 'data'
  gdonor <- intersect(names(glink), names(D))

  #-----

  # Read the necessary 'glink' variables from disk and set to keyed data.table
  gv <- unique(c(gtarget, gdonor))
  glink <- glink[c(gw, gv)] %>%
    mutate_if(is.character, factor) %>%
    data.table(key = gv) %>%
    setnames(c("W", gv))  # Rename the 'gw' variable to "W" for ease of use in data.table operations

  # Aggregate geographic weight to reduce number of observations
  glink <- glink[, .(W = sum(W)), by = gv]

  #---

  # Load the necessary geographic donor variables and convert to keyed data.table
  D <- D[c(did, gdonor)] %>%
    data.table(key = gdonor)

  # Ensure the factor levels are consistent between 'glink' and 'D' (possible that latter could be missing some levels)
  for (v in gdonor) set(D, j = v, value = factor(D[[v]], levels = levels(glink[[v]]), ordered = is.ordered(glink[[v]])))

  #---

  # Assign integer ID for each geographic intersection defined by the 'gdonor' variables
  D[, id := .GRP, by = gdonor]
  #setorder(D, id)  # NECESSARY?

  # Add 'id' assignment to 'glink' observations
  glink[D, id := i.id, on = gdonor]
  #setorder(glink, id) # NECESSARY?

  # If any 'puma10' are NA, that indicates a geo group in donor was not present in geolink
  stopifnot(!anyNA(glink$puma10))

  # Report number of unique geographic intersections
  cat("Identified", max(D$id), "geographic intersections in the donor...\n")

  #-----

  # CHECK FOR SAFETY - manual check that the 'id' assignment was correct
  c1 <- D %>%
    select(all_of(c(gdonor, "id"))) %>%
    distinct() %>%
    arrange(id)

  c2 <- glink %>%
    filter(!is.na(id)) %>%
    select(all_of(c(gdonor, "id"))) %>%
    distinct() %>%
    arrange(id)

  stopifnot(identical(c1, c2))

  #-----

  # For each PUMA, calculate proportion of households in each intersection
  # That is, conditional on a HH being in intersection 'id', what is the likelihood that it is located in each PUMA?
  glink[, puma_share := W / sum(W), by = gtarget]

  #-----

  # Donor output from harmonize() with 'id' merged
  #set(data, j = gdonor, value = NULL)
  D <- harmonized[[1]] %>%
    as.data.table() %>%
    merge(D, by = did)

  # Recipient output from harmonize() with 'id' merged
  R <- harmonized[[2]] %>%
    as.data.table()

  # Variables to use for distance/similarity calculation
  X <- setdiff(intersect(names(D), names(R)), "weight")

  #-----

  #i <- 5
  #N <- 1000

  sampleIntersection <- function(i, N = 1000) {

    d <- subset(D, id == i)

    # puma_share is the naive probability that the respondent is in intersection 'id'
    # When default sample weight is multiplied by 'puma_share', we get an estimate of the intersection-specific sample weight
    r <- R %>%
      merge(subset(glink, id == i, c(gtarget, "id", "puma_share")), by = gtarget) %>%
      mutate(weight = weight * puma_share) %>%
      slice_sample(n = min(N, nrow(.)))

    # Gower distance for top-n most similar respondents
    G <- gower::gower_topn(x = d[, ..X], y = r[, ..X], n = nrow(r), nthread = mc.cores)

    # Initial 'weight' of each recipient observation
    # This is the "naive" likelihood of selection
    P0 <- G$index
    P0[] <- r$weight[G$index]

    # Adjust any zero distance in 'G' to some arbitrary low value (avoid divide-by-zero errors)
    zero <- G$distance == 0
    if (any(zero)) G$distance[zero] <- 0.5 * min(G$distance[!zero])

    # Adjusted likelihood of selection
    P <- P0 / G$distance

    # Random sampling of 'm' implicates for each respondent in 'd', based on combined probability of selection (P0 * P1)
    S <- sapply(X = 1:ncol(P),
                FUN = function(i) sample(x = G$index[, i], size = m, replace = TRUE, prob = P[, i]),
                simplify = TRUE)

    # Assemble output; imputed (state, PUMA, recipient ID) and actual (gdonor) geographic variables for each donor household in 'd'
    dkeep <- c(did, gdonor)
    rkeep <- c(rid, gtarget)
    out <- cbind(d[rep(1:nrow(d), times = m), ..dkeep],
                 r[as.vector(t(S)), ..rkeep])

    return(out)

  }

  #---

  cat("Imputing PUMA for donor observations...\n")

  # Troubleshooting
  #for (id in sort(unique(D$id))) sampleIntersection(id)

  D <- pbapply::pblapply(X = sort(unique(D$id)),
                         FUN = sampleIntersection,
                         cl = 1L,  # This defaults to lapply() execution but gives a nice progress bar/timer
                         N = 1000) %>%
    rbindlist() %>%
    setcolorder(c(did, rid, gtarget, gdonor))

  # Calculate 'weight_adjustment' column
  # When 'D' is merged with microdata, the household "weight" is multiplied by "weight_adjustment" to arrive at correct total sample weight
  # This allow the unique() call below, which reduces the number of row in results (i.e. collapse duplicated entries)
  # Note that the recipient ID is dropped, which is OK if we don't care about adding additional ACS-based predictor variables (might be changed in future)
  data.table::set(D, j = rid, value = NULL)
  D[, weight_adjustment := .N / m, by = c(did, gtarget)]
  D <- unique(D)
  #setkeyv(D, cols = did)  # Set key so it can be merged quickly on 'did' within prepare()?

  gc()

  #---

  cat("Assigning location variables to recipient observations...\n")

  # Assign 'gdonor' variables to each recipient household
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

  # Add the 'N' count variable to 'glink'; this gives the number of households in each unique PUMA
  # For each PUMA in 'glink', randomly sample 'N' rows, where probability of selection is equal to 'puma_share'
  # The result is a data.table with same number of rows as 'R'
  # Order the rows by PUMA so it is aligned with 'R' and can be cbind'd below
  glink[R, N := i.N, on = gtarget]
  glink <- glink[glink[, .I[sample(.N, size = N, prob = puma_share, replace = TRUE)], by = gtarget]$V1]
  setorderv(glink, cols = gtarget)

  # Assign 'gdonor' variables for each recipient household
  stopifnot(nrow(R) == nrow(glink))
  R <- cbind(R[, ..rid], glink[, ..gdonor])
  #setkeyv(R, cols = rid)  # Set key so it can be merged quickly on 'rid' within prepare()?

  #---

  # Rename the 'gdonor' variables to include the "loc.." prefix
  # This identifies them as spatial variables, but "loc" is reserved for variables that are actually known and not imputed
  lvars <- paste0("loc..", gdonor)
  setnames(D, old = gdonor, new = lvars)
  setnames(R, old = gdonor, new = lvars)

  # Assemble into final results list
  # Assign attribute indicating the geographic intersection (i.e. "location") variables
  result <- list(D, R)
  names(result) <- names(harmonized)
  setattr(result, "location.vars", lvars)
  return(result)

}
