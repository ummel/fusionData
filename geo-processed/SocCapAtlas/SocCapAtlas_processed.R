library(tidyverse)
source("R/utils.R")

result <- read_csv("geo-raw/SocCapAtlas/social_capital_zip.csv") %>%
  mutate(zcta10 = str_pad(zip, 5, pad = 0),
         vintage = 2018L) %>% # Variables circa 2018?
  select(zcta10, vintage, ec_zip, ec_high_zip, nbhd_ec_zip, nbhd_ec_high_zip, nbhd_exposure_zip, nbhd_bias_zip, clustering_zip, support_ratio_zip, volunteering_rate_zip, civic_organizations_zip) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001)

saveRDS(result, "geo-processed/SocCapAtlas/SocCapAtlas_processed.rds")
