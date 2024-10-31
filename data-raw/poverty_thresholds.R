library(tidyverse)
library(readxl)
source("R/utils.R")

# U.S. Poverty Thresholds by Size of Family and Number of Children
# https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
poverty_thresholds <- lapply(1980:2023, function(year) {
  tf <- tempfile(fileext = ".xlsx")
  download.file(url = sub("YY", substring(year, 3, 4), "https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-thresholds/threshYY.xlsx"), destfile = tf)
  readxl::read_excel(tf, skip = 6, col_names = c('type', 'wa', paste0("m", 0:8))) %>%
    filter(!is.na(m0)) %>%
    mutate(wa = NULL,
           size = as.integer(c(1, 1, 2, 2, 3:9))) %>%
    pivot_longer(cols = -all_of(c('type', 'size')), values_drop_na = TRUE, values_transform = as.integer, values_to = "threshold", names_to = "minors") %>%
    mutate(year = as.integer(!!year),
           minors = as.integer(substring(minors, 2, 2)),
           senior = map(type, ~ if (grepl("people", .x)) {c(T, F)} else {c(grepl("65 years and over", .x))})) %>%
    unnest(senior) %>%
    select(year, size, minors, senior, threshold)
}) %>%
  bind_rows() %>%
  data.table(key = c('year', 'size', 'minors', 'senior'))

usethis::use_data(poverty_thresholds)
