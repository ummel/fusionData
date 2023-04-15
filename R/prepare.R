#' Prepare microdata inputs for assembly
#'
#' @description
#' Prepares data inputs to pass to \code{\link{assemble}}. Harmonizes common variables for the specified donor and recipient surveys, imputes PUMA for donor records, and samples location variables for recipient records.
#'
#' @param donor Character. Donor survey identifier (e.g. `"RECS_2015"`).
#' @param recipient Character. Recipient (ACS) survey identifier (e.g. `"ACS_2015"`).
#' @param respondent Character. Desired respondent level of microdata. Either `"household"` or `"person"`.
#' @param implicates Integer. Number of PUMA implicates to return for the donor microdata.
#' @param collapse Logical. Should rows be collapsed and weighting factors aggregated when there are multiple imputations of the same household-PUMA?
#' @param ncores Integer. Number of physical CPU cores used for parallel computation.
#'
#' @return A list of length two containing output data frames specific to donor and recipient output, respectfully. Can be passed to \code{\link{assemble}}.
#'
#' @examples
#' prep <- prepare(donor = "RECS_2015",
#'                 recipient = "ACS_2015",
#'                 respondent = "household",
#'                 implicates = 3)
#'
#' @export

#-----

# source("R/utils.R")
#
# Example usage
# test <- prepare(donor = "RECS_2015",
#                 recipient = "ACS_2015",
#                 respondent = "household",
#                 implicates = 5)
#
# Example inputs
# donor = "RECS_2015"
# recipient = "ACS_2015"
# respondent = "household"
# implicates = 5
#
# Possible future argument for spatially-explicit donor variables that match a spatial predictor
# spatial.harmony <- list(cdd65 = "climate..cddb6",
#                         hdd65 = "climate..hddb6",
#                         cdd30yr = "climate..cdd12b6",
#                         hdd30yr = "climate..hdd12b6")

# TEST
# donor = "ASEC_2019"
# recipient = "ACS_2019"
# respondent = "household"
# fuse = c("heatsub", "heatval", "incchild", "kidcneed", "hipval", "spmwic", "adjginc")
# implicates = 1
# collapse = FALSE
# ncores = getOption("fusionData.cores")

#-----

prepare <- function(donor,
                    recipient,
                    respondent,
                    implicates = 1,
                    collapse = FALSE,
                    ncores = getOption("fusionData.cores")) {

  # Validate arguments
  stopifnot({
    respondent %in% c("household", "person")
    implicates >= 1 & implicates %% 1 == 0
  })

  #-----

  # Harmonize data for specified donor and recipient surveys
  data <- harmonize(harmony.file = paste0(donor, "__", recipient, ".R"),
                    respondent = respondent,
                    ncores = ncores)

  #-----

  # Impute PUMA for the donor
  location.data <- assignLocation(harmonized = data,
                                  m = implicates,
                                  collapse = collapse,
                                  ncores = ncores)

  #----

  # Return output list
  return(list(harmonized = data, location = location.data))

}
