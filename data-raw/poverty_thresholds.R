# ==============================================================================
# DATA CREATION SCRIPT: Census Historical Poverty Thresholds
# ==============================================================================
# Purpose:
#   Downloads and reshapes official U.S. Census Bureau Historical Poverty
#   Threshold tables across multiple survey years into a standardized, tabular
#   lookup object.
#
# Role in package:
#   Generates the exported `poverty_thresholds` `data.table` stored in
#   `/data/*.rda`. It allows fast binary key lookup (`year`, `size`, `minors`,
#   `senior`) to assess family poverty status and construct federal poverty
#   line ratios across dynamic household compositions.
#
# Data Source:
#   U.S. Census Bureau Current Population Survey (CPS) Historical Poverty Thresholds:
#   https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#   Excel pattern: `https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-thresholds/threshYY.xlsx`
#
# Key Definitions:
#   - year: Survey/poverty reference year (2005 through 2024).
#   - size: Total family unit size (1 to 9+ members).
#   - minors: Number of related children under 18 years old (0 to 8+).
#   - senior: Logical indicator (`TRUE`/`FALSE`) for whether householder primary
#     reference is 65 years and over (relevant for 1 and 2-person units).
#   - threshold: Dollar amount establishing official poverty criteria.
# ==============================================================================

library(tidyverse)
library(readxl)
library(data.table)
source("R/utils.R")

# ------------------------------------------------------------------------------
# 1. Download & Reshape Annual Threshold Tables
# ------------------------------------------------------------------------------

# Fetch and standardize Census historical poverty Excel spreadsheets (2005-2024)
poverty_thresholds <- lapply(2005:2024, function(year) {

  # Target Excel URL string based on two-digit year representation
  url <- sub("YY", substring(year, 3, 4), "https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-thresholds/threshYY.xlsx")
  tf <- tempfile(fileext = ".xlsx")

  download.file(url = url, destfile = tf, quiet = TRUE)

  # Read and unpivot matrix into long schema
  readxl::read_excel(tf, skip = 6, col_names = c('type', 'wa', paste0("m", 0:8))) %>%
    filter(!is.na(m0)) %>%
    mutate(wa = NULL,
           size = as.integer(c(1, 1, 2, 2, 3:9))) %>%
    pivot_longer(cols = -all_of(c('type', 'size')),
                 values_drop_na = TRUE,
                 values_transform = as.integer,
                 values_to = "threshold",
                 names_to = "minors") %>%
    mutate(
      year = as.integer(!!year),
           minors = as.integer(substring(minors, 2, 2)),
           senior = map(type, ~ if (grepl("people", .x)) c(TRUE, FALSE) else c(grepl("65 years and over", .x)))
      ) %>%
    unnest(senior) %>%
    select(year, size, minors, senior, threshold)
}) %>%
  bind_rows() %>%
  data.table::data.table(key = c('year', 'size', 'minors', 'senior'))

# ------------------------------------------------------------------------------
# 2. Save Output to Package Data Directory
# ------------------------------------------------------------------------------

# Save keyed data.table object to package /data directory
use_data2(poverty_thresholds, overwrite = TRUE)
