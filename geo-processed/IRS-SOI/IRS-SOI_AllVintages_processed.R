# IRS Statistics of Income (SOI) by zip code
# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi

library(tidyverse)
source("R/utils.R")

#------------------

# Raw data download link templates
# Replace 'YR' with "18", for example
withoutagi.template <- "https://www.irs.gov/pub/irs-soi/YRzpallnoagi.csv"
withagi.template <- "https://www.irs.gov/pub/irs-soi/YRzpallagi.csv"

# Set timeout higher to allow download of large .csv file below
options(timeout = 180)

#------------------

processIRSSOI <- function(year) {

  # YEAR .csv data file, NOT classified by AGI category
  url <- sub("YR", substring(year, 3, 4), withoutagi.template)
  fpath <- paste0(file.path("geo-raw/IRS-SOI", year, basename(url)), ".gz")
  ondisk <- file.exists(fpath)

  if (!ondisk) {
    tf <- tempfile()
    download.file(url, tf)
    d <- read.csv(tf) %>%
      mutate_if(is.double, convertInteger)
  } else {
    d <- readr::read_csv(fpath)
  }

  #-----

  # Process the non-AGI data file to compute select variables
  noagi <- d %>%
    rename_all(toupper) %>%
    filter(!ZIPCODE %in% c(0, 99999)) %>%
    mutate(zcta10 = str_pad(ZIPCODE, width = 5, pad = 0))

  # Extract zip code summary variables
  # NOTE: SOI zip code data do not report total social security benefits (only the taxable portion), so this flow is ignored below
  irs1 <- noagi %>%
    mutate(`Mean income per return` = 1e3 * A02650 / N1,  # "Total income" (1040 line 22)
           `Mean income per person` = 1e3 * A02650 / (MARS1 + 2 * MARS2 + MARS4 + NUMDEP),  # "Total income" (1040 line 22)
           `Mean people per return` = (MARS1 + 2 * MARS2 + MARS4 + NUMDEP) / N1,
           `Mean dependents per return` = NUMDEP / N1,
           `Percent single returns` = MARS1 / (MARS1 + MARS2 + MARS4),
           `Percent joint returns` = MARS2 / (MARS1 + MARS2 + MARS4),
           `Percent head of household returns` = MARS4 / (MARS1 + MARS2 + MARS4),
           `Percent paid preparer returns` = PREP / N1,
           `Percent volunteer preparer returns` = TOTAL_VITA / N1,
           `Percent elderly returns` = ELDERLY / N1,
           `Percent farm returns` = SCHF / N1,
           `Percent EITC returns` = N59660 / N1,
           `Percent income earned` = (A00200 + A00900) / A02650,  # Salary/wages and net business income
           #`Percent income retirement` = (A01400 + A01700) / A02650,  # IRA distributions and pensions; does not include social security (see note above)
           `Percent income unemployment` = A02300 / A02650,
           `Effective tax rate` = (A10300 - A09400 - A59660 - A59720 - A11070 - A10960 - A11560) / (A02650 - A09400)
    ) %>%
    select(zcta10,  `Mean income per return`:`Effective tax rate`)

  # Total number of returns filed, by zip code (using the non-AGI data file)
  # This is merged with AGI data file below
  N1total <- noagi %>%
    mutate(returns = N1) %>%
    select(zcta10, returns)

  #-----

  # Save compressed raw .csv data to disk in appropriate sub-directory of /geo-raw
  # This data is saved for security only -- in (rare) event that IRS SOI deletes the historical data or it becomes inaccessible
  if (!ondisk) {
    dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(d, file = fpath)
    unlink(tf)
  }

  #---------------

  # YEAR .csv data files, WITH classification by AGI category
  # NOTE: LARGE FILE -- takes some time to download
  url <- sub("YR", substring(year, 3, 4), withagi.template)
  fpath <- paste0(file.path("geo-raw/IRS-SOI", year, basename(url)), ".gz")
  ondisk <- file.exists(fpath)

  if (!ondisk) {
    tf <- tempfile()
    download.file(url, tf)
    d <- read.csv(tf) %>%
      mutate_if(is.double, convertInteger) %>%
      select(zipcode, agi_stub, N1)  # Retain ONLY the variables used below
  } else {
    d <- readr::read_csv(fpath)
  }

  #-----

  # Process the AGI data file to compute select variables
  irs2 <- d %>%
    rename_all(toupper) %>%
    mutate(zcta10 = str_pad(ZIPCODE, width = 5, pad = 0),
           agi_category = paste0("Percent AGI ", c("Less than $25k", "$25k to $50k", "$50k to $75k", "$75k to $100k", "$100k to $200k", "$200k or more"))[AGI_STUB],
           agi_category = factor(agi_category, levels = unique(agi_category))) %>%
    inner_join(N1total, by = "zcta10") %>%
    mutate(agi_share = N1 / returns) %>%
    select(zcta10, agi_category, agi_share) %>%
    spread(agi_category, agi_share) %>%
    na.omit() # Remove rows with any NA values; this indicates data suppression/grouping occurred across AGI categories and the results cannot be clearly interpreted

  # Safety check: Check the row sums (returns TRUE is all rows sum to 1)
  stopifnot(all(round(rowSums(irs2[,-1]), 3) == 1))

  #-----

  # Save compressed raw .csv data to disk in appropriate sub-directory of /geo-raw
  # This data is saved for security only -- in (rare) event that IRS SOI deletes the historical data or it becomes inaccessible
  if (!ondisk) {
    dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(d, file = fpath)
    unlink(tf)
  }

  #-----

  # Compile the final results across all zip codes
  result <- left_join(irs1, irs2, by = "zcta10") %>%
    mutate_if(is.double, cleanNumeric) %>%
    mutate_if(is.double, convertInteger) %>%
    mutate(vintage = as.integer(year)) %>%
    select(zcta10, vintage, everything())

  # Save result to /geo-processed as .rds file
  fpath <- file.path("geo-processed/IRS-SOI", paste0("IRS-SOI_", year, "_processed.rds"))
  dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, file = fpath)

}

#------------------

# Using function above to download, process, and save a new vintage of IRS-SOI data
# See here for available years: https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi
# processIRSSOI(year = 2018)
# processIRSSOI(year = 2017)
# processIRSSOI(year = 2016)
# processIRSSOI(year = 2015)
#NOT RUN: processIRSSOI(year = 2014)  # Not all hard-coded variables used above are available prior to 2015
