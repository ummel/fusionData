#' Impute PUMA for survey records
#'
#' @description
#' Imputes the PUMA associated with survey records by randomly drawing a PUMA from the geographic area defined by available geographic identifiers for a given record. `m` PUMA's are randomly drawn for each record, where the probability of selection is proportional to the number of housing units in each PUMA.
#'
#' @return A data frame.
#'
#' @examples
#' imputePUMA(suvey = "RECS_2015", m = 1)
#'
#' @export

#-----

imputePUMA <- function(survey, m = 1, safely = TRUE) {

  # require(data.table)
  # require(tidyverse)

  # Variables in geolink defining the "target" geography (i.e. uniquely-identified PUMA's)
  gtarget <- c("state", "puma10")

  # The PUMA-related weight variable in 'glink' (i.e. housing unit count)
  gw <- "puma_weight"

  #-----

  # Soft load the geolink.fst file
  glink <- fst::fst("geo-processed/concordance/geo_concordance.fst")

  # Soft load the specified 'survey' processed .fst file
  temp <- list.files(path = "survey-processed", pattern = paste0("^", survey, "_._processed.fst"), recursive = TRUE, full.names = TRUE)
  temp <- temp[1L]  # This retains the household file in event that 'survey' has both H and P data (only H needed to sample PUMAs)
  if (length(temp) == 0) stop("No microdata available for ",  survey, " survey")
  data <- fst::fst(temp)

  #-----

  # Number of input observations in 'data'
  N <- nrow(data)

  # Identify the 'hid' variable
  hid <- grep(paste0("^.*_.*_hid$"), names(data), value = TRUE)

  # Identify geographic variables in 'data'
  gdonor <- intersect(names(glink), names(data))

  #---

  # Load the necessary 'data' variables and set to keyed data.table
  data <- data[c(hid, gdonor)] %>%
    mutate_at(gdonor, as.character) %>%
    data.table(key = gdonor)

  # Assign integer ID for each geographic group
  data[, id := .GRP, by = gdonor]
  setorder(data, id)

  # Number of unique geographic intersections
  max(data$id)

  #-----

  # Load the necessary 'glink' variables and set to keyed data.table
  gv <- unique(c(gtarget, gdonor))
  glink <- glink[c(gw, gv)] %>%
    setnames(c("W", gv)) %>%  # Rename the 'gw' variable to "W" for ease of use in data.table operations
    data.table(key = gv)

  # Aggregate geographic weight
  glink <- glink[, .(W = sum(W)), by = gv]

  # Add 'id' assignment to 'glink' observations
  glink[data, id := i.id, on = gdonor]
  glink <- glink[!is.na(id), ]

  # If any 'puma10' are NA, that indicates a geo group in donor was not present in geolink
  stopifnot(!anyNA(glink$puma10))

  #-----

  # CHECK FOR SAFETY
  # CAN BE TURNED OFF EVENTUALLY
  if (safely) {

    c1 <- data %>%
      select(all_of(c(gdonor, "id"))) %>%
      distinct() %>%
      arrange(id)

    c2 <- glink %>%
      select(all_of(c(gdonor, "id"))) %>%
      distinct() %>%
      arrange(id)

    stopifnot(identical(c1, c2))

  }

  #-----

  # Randomly sample 'm' PUMA's for each household, by ID

  # Number of observations in 'data' for each value of 'id'
  idn <- tapply(data$id, data$id, length)

  # Sample 'gtarget' variables...
  samp <- lapply(seq_along(idn), function(i) {
    s <- glink[id == i, ]
    x <- sample.int(n = nrow(s),
                    size = idn[i] * m,
                    replace = TRUE,
                    prob = s$W)
    s[x, ..gtarget]
  })

  # 'm' puma's assigned to each 'hid'
  ind <- as.vector(mapply(rep, 1:nrow(data), each = m))
  result <- cbind(data[ind, ..hid], rbindlist(samp))

  # Safety check for correct number of output rows prior to collapsing unique entries
  stopifnot(nrow(result) == N * m)

  # Calculate 'weight_adjustment' column
  # When 'result' is merge with microdata, the household "weight" is multiplied by "weight_adjustment" to arrive at correct total sample weight
  # This allow the unique() call below, which reduces the number of row in results (i.e. collapse duplicated entries)
  result[, weight_adjustment := .N / m, by = names(result)]
  result <- unique(result)
  setorderv(result, hid)

  # Assign attribute with the geographic identifiers
  attr(result, "geo.vars") <- gdonor

  return(result)

}
