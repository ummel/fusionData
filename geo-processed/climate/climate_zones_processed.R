library(tidyverse)

#------------------

# Building America and IECC climate zones
# https://codes.iccsafe.org/public/document/IECC2015/iecc-residential-provisions
# https://www.energy.gov/sites/prod/files/2015/10/f27/ba_climate_region_guide_7.3.pdf

# One-time download of climate zones .csv file from philngo github repo
# https://gist.github.com/philngo/d3e251040569dba67942
# download.file(url = "https://gist.github.com/philngo/d3e251040569dba67942/raw/0c98f906f452b9c80d42aec3c8c3e1aafab9add8/climate_zones.csv",
#               destfile = "geo-raw/climate/climate_zones.csv")

# NOTE: The climate zones in the file above (as of June 18, 2021) use the 2003 IECC definitions.
# The IECC codes underwent a revision in 2021: https://www.jm.com/en/blog/2021/march/understanding-the-iecc-s-new-climate-zone-map/

#------------------

# Process raw county-zone data
d <- "geo-raw/climate/climate_zones.csv" %>%
  read.csv(colClasses = "character", na.strings = "N/A") %>%
  rename(state = State.FIPS,
         county10 = County.FIPS,
         ba_zone = BA.Climate.Zone) %>%
  mutate(iecc_zone = paste0(IECC.Climate.Zone, ifelse(is.na(IECC.Moisture.Regime), '', IECC.Moisture.Regime),  ifelse(ba_zone == 'Hot-Humid', '*', '')),
         vintage = "always") %>%
  select(state, county10, vintage, iecc_zone, ba_zone)

# Set variable labels
d <- labelled::set_variable_labels(d, .labels = c("State code", "County code (2010)", "Vintage", "IECC climate zone", "Building America climate zone"))

# Save processed data to disk
saveRDS(d, "geo-processed/climate/climate_zones_processed.rds")
