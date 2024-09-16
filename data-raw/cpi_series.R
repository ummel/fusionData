# Set API key for FRED data access
library(fredr)
fred.key <- "db2083a77884063c197a2529f7f9e4d2"
fredr_set_key(fred.key)

# Obtain BLS FRED CPI data and summarize annually
cpi_series <- fredr(series_id = "CPIAUCSL") %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  add_count(year) %>%
  filter(n == 12) %>%  # Restrict to years with full, 12-month data
  group_by(year) %>%
  summarize(cpi = mean(value), .groups = "drop") %>%
  mutate(cpi = cpi[year == max(year)] / cpi)  # Computes index relative to latest year of full data

# Save result to /data
usethis::use_data(cpi_series, overwrite = TRUE)
