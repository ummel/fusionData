library(tidyverse)
library(RSocrata)

# CDC OData URL for the PLACES 2023 dataset
# This was obtained by going to: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh
#   and clicking the three dots drop down in top right and selecting "Access Data via OData"
# Using this approach rather than download and store the raw data, because the raw data file is quite large
url <- "https://data.cdc.gov/api/odata/v4/cwsq-ngmh"
d <- read.socrata(url)

# The 'year' variable (either 2020 or 2021) indicates the vintage of the donor survey from which the variable comes
# Below, I assign vintage "2020-2021" to cover both of these values
places <- d %>%
  select(locationid, measureid, data_value) %>%
  pivot_wider(names_from = measureid, values_from = data_value) %>%
  mutate(locationid = str_pad(locationid, width = 11, pad = 0),
         state = substring(locationid, 1, 2),
         county10 = substring(locationid, 3, 5),
         tract10 = substring(locationid, 6, 11),
         vintage = "2020-2021") %>%  # See note above
  select(state, county10, tract10, vintage, ACCESS2:VISION) %>%
  rename_with(tolower, ACCESS2:VISION)

saveRDS(places, "geo-processed/CDC-PLACES/CDC-PLACES_2023_processed.rds")
