#' Block group centroids circa 2010
#'
#' Population-weighted block group centroids. \code{sf} points object. Useful for assigning any coordinate-based spatial features to block groups to create geographic concordance; e.g. using \code{\link[sf]{st_nearest_feature}}.
#'
#' @format A \code{sf} spatial data frame.
#' \describe{
#'   \item{state}{State code}
#'   \item{county10}{County code}
#'   \item{tract10}{Tract code}
#'   \item{bg10}{Block group code}
#'   \item{geometry}{Centroid coordinates as \code{`sfc_POINT`} class}
#'   }
#' @source \url{https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/}
"bg_centroids"
