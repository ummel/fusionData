library(tidyverse)

sld <- fst::read_fst("geo-raw/EPA-SLD/v3.0/SmartLocationDatabaseV3.fst") %>%
  rename(state = STATEFP,
         county10 = COUNTYFP,
         tract10 = TRACTCE,
         bg10 = BLKGRPCE) %>%
  mutate(vintage = 2018L) %>% # Most variables are circa 2018
  select(state, county10, tract10, bg10, vintage, Ac_Total:SLC_score)

saveRDS(sld, "geo-processed/EPA-SLD/epa-sld_v3_processed.rds")




