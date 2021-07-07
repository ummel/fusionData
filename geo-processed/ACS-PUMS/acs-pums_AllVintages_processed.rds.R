library(tidyverse)
source("R/utils.R")

#----------------------------------

# Function to generate processed .rds file for a given year
summarizePUMS <- function(year) {

  data <- fst::fst(gsub("YEAR", year, "survey-processed/ACS/YEAR/ACS_YEAR_H_processed.fst"))

  # Observation weight
  W <- data[["weight"]]

  # PUMA identifier
  P <- paste0(data[["state"]], data[["puma10"]])

  # Index giving location of observations in each PUMA
  index <- split(1:length(P), P)

  # Variables to process
  vars <- setdiff(names(data)[-c(1, 2)], c("puma10", "state"))
  vars <- grep("^rep_*", vars, invert = TRUE, value = TRUE)

  #-----

  # Function to calculate PUMA-wise weighted means for a given vector 'x'
  wmFun <- function(x) sapply(index, function(i) weighted.mean(x = x[i], w = W[i]))

  #-----

  # Compute summary statistics for variable 'v'
  processVar <- function(v) {

    d <- data[[v]]

    if (is.numeric(d)) {

      # If there are numerous unique values (> 100), return median; otherwise return mean
      out <- if (length(unique(d)) > 100) {
        sapply(index, function(i) weightedQuantile(x = d[i], w = W[i], p = 0.5))
      } else {
        wmFun(d)
      }
      out <- setNames(data.frame(out), v)

    } else {

      # Lump factor levels
      d <- forcats::fct_lump_prop(d, prop = 0.05, w = W)  # Lump categories with less than 5% overall sample weight
      u <- levels(d)
      u <- u[-length(u)]  # Drop the final level; not necessary since the proportions otherwise sum to one, by row
      newv <- paste(v, betterAbbreviate(u), sep = "_")

      if (length(newv) > 0) {

        # Create dummy matrix
        dum <- matrix(NA, nrow = length(d), ncol = length(newv)) %>%
          as.data.frame() %>%
          setNames(newv)
        data.table::set(dum, j = newv, value = lapply(u, function(x) as.integer(d == x)))

        # Calculate PUMA-wise weighted proportion for each category
        out <- as.data.frame(apply(dum, MARGIN = 2, FUN = wmFun))

      } else {
        out <- NULL
      }

    }

    return(out)

  }

  #-----

  # Process each variable (in parallel)
  result <- pbapply::pblapply(X = vars, FUN = processVar, cl = 3L)

  # Assemble and clean-up final data frame
  result <- do.call(cbind, result) %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    select_if(~ novary(.x) == FALSE) %>%  # Remove variables without variation
    mutate(state = str_sub(names(index), 1, 2),
           puma10 = str_sub(names(index), 3, -1),
           vintage = as.integer(year)) %>%
    as_tibble() %>%
    select(state, puma10, vintage, everything())

  # Save result to disk
  saveRDS(result, file = sub("YEAR", year, "geo-processed/ACS-PUMS/acs-pums_YEAR_processed.rds"))

}

#----------------------------------

# Can only call summarizePUMS() for year for which we have processed microdata
summarizePUMS(2019)
summarizePUMS(2015)
