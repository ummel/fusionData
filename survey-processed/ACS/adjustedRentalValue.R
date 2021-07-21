# Example inputs
# renteq <- d$renteq
# value <- d$VALP
# state <- d$ST
# structure <- d$BLD
# bedrooms <- d$BDSP
# weight <- d$WGTP

adjustedRentalValue <- function(renteq, value, state, structure, bedrooms, weight) {

  clipFun <- function(x, cumprop = 0.9) {
    x[x == 0] <- 1L
    p <- cumsum(table(x) / length(x))
    i <- max(which(p <= cumprop))
    x[x > i] <- i
    return(x)
  }

  # Prepare input data set; create state-level "Other" category for infrequent (<2.5% of cases) 'structure' and 'bedrooms' values
  dset <- data.frame(renteq, value, state, structure, bedrooms) %>%
    mutate(structure = as.character(structure),
           structure = ifelse(grepl("One-family", structure), "Single", structure),
           structure = ifelse(grepl("Mobile", structure) | grepl("Boat", structure), "Mobile", structure),
           structure = ifelse(!structure %in% c("Single", "Mobile"), "Multi", structure)) %>%
    group_by(state) %>%
    mutate(bedrooms = ifelse(structure == "Mobile", 0, clipFun(bedrooms))) %>%
    ungroup()

  # Calculate median home value (as reported by home owners) by state-structure-bedrooms stratum
  mval <- dset %>%
    filter(value > 0) %>%  # Exclude observations where no property value is supplied
    group_by(state, structure, bedrooms) %>%
    summarize(median_value = weightedQuantile(value, w = weight, p = 0.5), .groups = "drop")

  adj <- dset %>%
    left_join(mval, by = c("state", "structure", "bedrooms")) %>%
    mutate(r = ifelse(value > 0, value / median_value, NA),
           adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
           adj = ifelse(value == 0, 1, adj)) %>%   # If no property value supplied, set 'adj' to 1 (i.e. renter-occupied unit)
    pull(adj)

  return(renteq * adj)

}

