#' Download processed spatial data
#'
#' @description
#' Since processed spatial data files are generally too large to upload to Github or attach to the fusionData package, they are stored on Google Drive. This function downloads requested processed geographic data and places it in the appropriate local sub-directory of /fusionData/geo_processed. The full file path (e.g. intermediate directories) are created, if necessary, but no existing data on disk is altered or deleted.
#'
#' @param dataset Character. Indicate which spatial dataset(s) to download. If \code{dataset = "essential"}, only files necessary to call \link{prepare} and \link{assemble} are downloaded. If \code{dataset = "all"}, all processed spatial data files are downloaded. Otherwise, a single spatial dataset identifier (e.g. "EPA-SLD") to download all associated "*_processed.rds" files.
#'
#' @details Files are automatically placed in appropriate sub-directory of fusionData/geo_processed. When \code{dataset = "essential"}, the downloaded files are: "geo_predictors.fst" and "concordance/geo_concordance.fst".
#'
#' @return \code{\link[googledrive]{drive_download}} prints messages to console indicating which files were downloaded.
#'
#' @examples
#' getGeoProcessed(dataset = "essential")
#'
#' @export

getGeoProcessed <- function(dataset = "essential") {

  # Identify files to be downloaded

  # Identify the "essential" files first, if necessary
  if (dataset %in% c("all", "essential")) {
    files <- lapply(c("geo_predictors.fst", "geo_concordance.fst", "bg_centroids.rds"),
                    function(x) googledrive::drive_find(pattern = paste0("^", x, "$"))) %>%
      bind_rows() %>%
      googledrive::drive_reveal("path") %>%
      mutate(path = gsub("~/fusionData/", "", path))
  } else {
    files <- NULL
  }

  # Add on additional files, as necessary
  if (dataset != "essential") {
    search <- if (dataset == "all") {
      "^.*_processed.rds$"
    } else {
      paste0("^", dataset, ".*_processed.rds$")
    }
    temp <- googledrive::drive_find(search) %>%
      googledrive::drive_reveal("path") %>%
      mutate(path = gsub("~/fusionData/", "", path))
    files <- rbind(files, temp)
  }

  #-----

  # Download data files
  for (i in 1:nrow(files)) {
    dir.create(dirname(files[i, ]$path), recursive = TRUE, showWarnings = FALSE)
    googledrive::drive_download(file = files[i, ],
                                path = files[i, ]$path,
                                overwrite = TRUE,
                                verbose = TRUE)
  }

  # NOTE: Get file modified time -- there is also a created time
  #files$drive_resource[[1]]$modifiedTime

}
