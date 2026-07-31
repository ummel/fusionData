# ==============================================================================
# DATA CREATION SCRIPT: Consumer Price Index (CPI) Series & Inflation Index
# ==============================================================================
# Purpose:
#   Downloads monthly Consumer Price Index data (CPI-U: All Urban Consumers)
#   from FRED (Federal Reserve Economic Data) and calculates annual average
#   inflation adjustment factors relative to the most recent complete year.
#
# Role in package:
#   Generates the exported `cpi_series` data frame stored in `/data/*.rda`.
#   This lookup dataset is used across data preparation pipelines (e.g., in
#   harmonization routines) to convert nominal monetary values (like household
#   income or expenditures) across different survey years into real, inflation-
#   adjusted dollars corresponding to a baseline evaluation year.
#
# Data Source:
#   FRED Series ID: "CPIAUCSL" (Consumer Price Index for All Urban Consumers:
#   All Items in U.S. City Average, Seasonally Adjusted).
#
# Calculation Details:
#   1. Monthly values are grouped by calendar year.
#   2. Incomplete years (fewer than 12 monthly observations) are filtered out.
#   3. The annual average CPI is computed for each full year: `mean(value)`.
#   4. An adjustment ratio is calculated as: `cpi_latest_full_year / cpi_year`.
#      Multiplying nominal dollars from year `t` by this index converts them to
#      constant dollars of the baseline year (`max(year)`).
# ==============================================================================

library(fredr)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. API Setup & Data Retrieval
# ------------------------------------------------------------------------------

# Set API key for FRED data access
fred.key <- "db2083a77884063c197a2529f7f9e4d2"
fredr_set_key(fred.key)

# ------------------------------------------------------------------------------
# 2. Process Monthly CPI into Annual Adjustment Index
# ------------------------------------------------------------------------------

# Obtain BLS FRED CPI data and summarize annually
cpi_series <- fredr(series_id = "CPIAUCSL") %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  add_count(year) %>%
  filter(n == 12) %>%  # Restrict to years with full, 12-month data
  group_by(year) %>%
  summarize(cpi = mean(value), .groups = "drop") %>%
  mutate(cpi = cpi[year == max(year)] / cpi)  # Computes index relative to latest year of full data

# ------------------------------------------------------------------------------
# 3. Save Output to Package Data Directory
# ------------------------------------------------------------------------------

# Save result to /data
usethis::use_data(cpi_series, overwrite = TRUE)
