#' Internal datasets available in fusionData
#'
#' An overview of internal reference datasets bundled with the package.
#'
#' @name datasets
#'
#' @details
#' \describe{
#'   \item{\code{\link{bg_centroids_2010}}}{Population-weighted block group centroids (2010 Census).}
#'   \item{\code{\link{bg_centroids_2020}}}{Population-weighted block group centroids (2020 Census).}
#'   \item{\code{\link{bg_crosswalk}}}{Geographic crosswalk between 2010 and 2020 block groups.}
#'   \item{\code{\link{BEA_pce_national}}}{BEA national Personal Consumption Expenditures series (1959–2020).}
#'   \item{\code{\link{BEA_pce_state}}}{BEA state-level Personal Consumption Expenditures series (1997–2019).}
#'   \item{\code{\link{cpi_series}}}{Annual Consumer Price Index series from FRED.}
#'   \item{\code{\link{poverty_thresholds}}}{Historical official U.S. Census Bureau poverty thresholds.}
#'   \item{\code{\link{puma_crosswalk}}}{Geographic crosswalk between 2010 and 2020 PUMAs.}
#' }
#'
#' @docType data
#' @keywords datasets
NULL

#-----------

#' Block group centroids circa 2010
#'
#' Population-weighted block group centroids. \code{sf} points object. Useful for assigning any coordinate-based spatial features to block groups to create geographic concordance; e.g. using \code{\link[sf]{st_nearest_feature}}.
#'
#' @format A \code{sf} spatial data frame with 6 variables:
#' \describe{
#'   \item{state}{State FIPS code (character)}
#'   \item{county10}{2010 County FIPS code (character)}
#'   \item{tract10}{2010 Census tract code (character)}
#'   \item{bg10}{2010 Block group code (character)}
#'   \item{pop10}{2010 total population count (integer)}
#'   \item{geometry}{Centroid coordinates as \code{sfc_POINT} class}
#' }
#' @source \url{https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/}
#' @keywords internal
"bg_centroids_2010"

#' Block group centroids circa 2020
#'
#' Population-weighted block group centroids. \code{sf} points object. Useful for assigning any coordinate-based spatial features to block groups to create geographic concordance; e.g. using \code{\link[sf]{st_nearest_feature}}.
#'
#' @format A \code{sf} spatial data frame with 6 variables:
#' \describe{
#'   \item{state}{State FIPS code (character)}
#'   \item{county20}{2020 County FIPS code (character)}
#'   \item{tract20}{2020 Census tract code (character)}
#'   \item{bg20}{2020 Block group code (character)}
#'   \item{pop20}{2020 total population count (integer)}
#'   \item{geometry}{Centroid coordinates as \code{sfc_POINT} class}
#' }
#' @source \url{https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/}
#' @keywords internal
"bg_centroids_2020"

#' National Personal Consumption Expenditures (PCE)
#'
#' Bureau of Economic Analysis (BEA) national-level Personal Consumption Expenditures price index and series from 1959 to 2020 used for macroeconomic deflating and scaling.
#'
#' @format A \code{data.table} data frame with 73 variables:
#' \describe{
#'   \item{line}{BEA line item number (integer)}
#'   \item{pce_desc}{Description of the PCE line item category (character)}
#'   \item{pce_series}{BEA series identification code (character)}
#'   \item{parent1}{Hierarchical parent series ID level 1 (character)}
#'   \item{parent2}{Hierarchical parent series ID level 2 (character)}
#'   \item{parent3}{Hierarchical parent series ID level 3 (character)}
#'   \item{parent4}{Hierarchical parent series ID level 4 (character)}
#'   \item{parent5}{Hierarchical parent series ID level 5 (character)}
#'   \item{parent6}{Hierarchical parent series ID level 6 (character)}
#'   \item{parent7}{Hierarchical parent series ID level 7 (character)}
#'   \item{parent8}{Hierarchical parent series ID level 8 (character)}
#'   \item{1959-2020}{Annual PCE expenditure/index values by year column (integer)}
#' }
#' @source U.S. Bureau of Economic Analysis (BEA).
#' @keywords internal
"BEA_pce_national"

#' State Personal Consumption Expenditures (PCE)
#'
#' Bureau of Economic Analysis (BEA) state-level Personal Consumption Expenditures price index and series from 1997 to 2019.
#'
#' @format A \code{data.table} data frame with 31 variables:
#' \describe{
#'   \item{line}{BEA line item number (integer)}
#'   \item{state_fips}{2-digit state FIPS code (character)}
#'   \item{state_name}{State name (character)}
#'   \item{pce_desc}{Description of the PCE line item category (character)}
#'   \item{pce_series}{BEA series identification code (character)}
#'   \item{parent1}{Hierarchical parent series ID level 1 (character)}
#'   \item{parent2}{Hierarchical parent series ID level 2 (character)}
#'   \item{parent3}{Hierarchical parent series ID level 3 (character)}
#'   \item{1997-2019}{Annual state-level PCE expenditure/index values by year column (numeric/integer)}
#' }
#' @source U.S. Bureau of Economic Analysis (BEA).
#' @keywords internal
"BEA_pce_state"

#' Consumer Price Index (CPI) Series
#'
#' Annual Consumer Price Index data sourced from Federal Reserve Economic Data (FRED) for price level and inflation adjustments across survey years.
#'
#' @format A \code{tbl_df} tibble data frame with 2 variables:
#' \describe{
#'   \item{year}{4-digit year (integer)}
#'   \item{cpi}{Annual CPI index value (numeric)}
#' }
#' @source Federal Reserve Bank of St. Louis (FRED) API.
#' @keywords internal
"cpi_series"

#' Historical Official Poverty Thresholds
#'
#' Historical official U.S. Census Bureau poverty thresholds organized by family size, number of related minor children, senior status, and year.
#'
#' @format A \code{data.table} data frame with 5 variables:
#' \describe{
#'   \item{year}{4-digit calendar year (integer)}
#'   \item{size}{Total family unit size (integer)}
#'   \item{minors}{Number of related minor children under 18 years (integer)}
#'   \item{senior}{Logical flag indicating if the householder is 65 years or older for single and two-person units (logical)}
#'   \item{threshold}{Official dollar poverty threshold (integer)}
#' }
#' @source U.S. Census Bureau, Current Population Survey (CPS) Poverty Thresholds.
#' @keywords internal
"poverty_thresholds"

#' PUMA Geographic Crosswalk
#'
#' Geographic relationship and crosswalk table linking Public Use Microdata Areas (PUMAs) across 2010 and 2020 Census definitions.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{state}{2-digit state FIPS code (character)}
#'   \item{puma20}{2020 5-digit PUMA code (character)}
#'   \item{puma10}{2010 5-digit PUMA code (character)}
#'   \item{xwalk_weight}{Population (2020) of the geographic intersection (integer)}
#' }
#' @source 2010-2020 PUMA NHGIS crosswalk (https://usa.ipums.org/usa/volii/pumas20.shtml)
#' @keywords internal
"puma_crosswalk"

#' Block Group Geographic Crosswalk
#'
#' Geographic relationship and crosswalk table linking block groups across 2010 and 2020 Census definitions.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{bg10}{2010 12-digit block group GEOID (character)}
#'   \item{bg20}{2020 12-digit block group GEOID (character)}
#'   \item{xwalk_weight}{Population (2020) of the geographic intersection (integer)}
#' }
#' @source 2020 Block Groups to 2010 Block Groups NHGIS crosswalk (https://www.nhgis.org/geographic-crosswalks)
#' @keywords internal
"bg_crosswalk"
