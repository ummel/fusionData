library(tidyverse)
source("R/utils.R")

#-----

# URDB pages:
# The full data base has rate structure and fixed cost variables; unclear exactly how to use then, however
# https://openei.org/wiki/Utility_Rate_Database
# https://data.openei.org/submissions/5
# urdb <- readr::read_csv("https://openei.org/apps/USURDB/download/usurdb.csv.gz")

# Currently used:
# Pre-processed zip code electricity rates by zip code
# Example URL: 2019 zip code rates
# https://data.openei.org/submissions/4042

#-----

# Function to download, process, and save zip code electricity prices to disk

getElecPrices <- function(year, url.tag) {

  # IOU utilities
  url1 <- paste0("https://data.openei.org/files/", url.tag, "/iou_zipcodes_", year, ".csv")

  # Non-IOU utilities
  url2 <- paste0("https://data.openei.org/files/", url.tag, "/non_iou_zipcodes_", year, ".csv")

  # Attempt to download data from both URLs
  data <- suppressWarnings(try(bind_rows(read.csv(url1), read.csv(url2)), silent = TRUE))

  # If initial attempt fails, try without underscores removed from URLs
  if (class(data) == "try-error") {
    data <- bind_rows(read.csv(gsub("_", "", url1)), read.csv(gsub("_", "", url2)))
  }

  # Calculate median $ per MWh of utilities operating in each zip code, residential rate
  # Ideally, this might be a weighted mean based on the number of utility companies, but that would require additional data
  result <- data %>%
    mutate(zcta10 = str_pad(zip, width = 5, pad = 0),
           vintage = year) %>%
    group_by(zcta10, vintage) %>%
    summarize(residential_electricity_dolpermwh = median(1000 * res_rate[res_rate > 0], na.rm = TRUE), .groups = "drop") %>%
    na.omit() %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001)

  # Save result to disk
  saveRDS(result, paste0("geo-processed/NREL-URDB/nrel-urdb_elec_prices_", year, "_processed.rds"))

}

#-----

# Run function for each available year
getElecPrices(year = 2019, 4042)
getElecPrices(year = 2018, 449)
getElecPrices(year = 2017, 448)
getElecPrices(year = 2016, 447)
getElecPrices(year = 2015, 446)
getElecPrices(year = 2014, 445)
getElecPrices(year = 2013, 444)
