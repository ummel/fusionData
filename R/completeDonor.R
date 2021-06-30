#' Add variables to harmonized donor microdata
#'
#' @description
#' Add donor variables to (i.e. complete) harmonized microdata, excluding variables that are harmonized with the recipient. This is useful for augmenting harmonized donor microdata produced by \link{harmonize} in preparation for passing to \code{\link[fusionModel]{train}}.
#'
#' @param data Data frame. Donor microdata, usually produced by \link{harmonize}.
#' @param ... Optional arguments passed to \code{\link[dplyr]{select}} for selecting (or excluding) particular donor variables.
#' @param replicates Logical. Should replicate observation weights be included, if available? Defaults to FALSE.
#'
#' @return A data frame containing all of the harmonized variables, along with all or some of the original (unharmonized) donor variables.
#'
#' @examples
#'
#' @export

# TO DO: Possibly add variable labels in output?

completeDonor <- function(data, ..., replicates = FALSE) {

  # Helper function (move to utils?)
  splitNames <- function(x) strsplit(x, "__", fixed = TRUE)

  # Household identifier
  survey <- attr(data, "survey")
  s <- strsplit(tolower(survey), "_")[[1]][1]
  hid <- grep(paste0("^", s, "_.*hid$"), names(data), value = TRUE)

  # Load household data (NULL if unavailable or unnecessary)
  fpath <- list.files(path = "survey-processed", pattern = paste(survey, ifelse("pid" %in% names(data), "P", "H"), "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
  d <- fst(fpath)

  # Observation ID variable(s)
  ids <- intersect(c(hid, "pid"), names(d))

  # Harmonized donor variables
  vharm <- setdiff(map_chr(splitNames(names(data)), 1), ids)

  # Variables to keep
  reps <- grep("^rep_\\d+$", names(d), value = TRUE)  # Replicate weights (if present)
  keep <- c(ids, setdiff(names(d), c(ids, vharm, reps)))
  if (!replicates) reps <- NULL

  # Load only the necessary data from disk
  # NOTE: This doesn't take into account '...', so it potentially loading unnecessary data
  d <- d[c(keep, reps)]

  # Pass ... argument as a dplyr select() statement
  # Retain ID variables and (optionally) replicate weights
  if (!missing(...)) d <- dplyr::select(d, ..., any_of(c(ids, "weight", reps)))

  # Add original donor variables to 'data'
  # Merge on household- and person-identifier(s)
  result <- data %>%
    inner_join(d, by = ids) %>%
    select(any_of(c(ids, "weight")), everything())
    #select(any_of(c(ids, "weight", names(data), names(d))))

  # Add 'fuse.vars' attribute to identify the variables that are fusion candidates
  attr(result, "fusion.vars") <- setdiff(names(d), c(ids, "weight", reps))
  attr(result, "replicate.vars") <- reps

  return(result)

}
