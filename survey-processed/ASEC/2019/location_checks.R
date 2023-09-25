## Checking location variables 
## created 8/9/22
library(pacman)
p_load(ipumsr, fusionData, here, data.table)

# all data from the CPS

# set to raw data directory for read_ipums commands to work
setwd('./survey-raw/ASEC/2019/')

# when reading in, convert variable names to lower
ddi <- read_ipums_ddi("cps_00057.xml", lower_vars = T)
d <- read_ipums_micro(ddi)
d <- as.data.table(d)

setwd(here())

# read in geo-concordance
geo <- fst::read_fst(path = 'geo-processed/concordance/geo_concordance.fst') 

# ~ counties ----

# in CPS, not all counties are identified - only about 45% are identified 
# list of available counties: https://cps.ipums.org/cps/codes/county_2005onward_codes.shtml
# Q: should these be changed to missing? Or left as 0

# compare county levels
n_distinct(geo$county10)
n_distinct(d$county)

d[county !=0, county] %>% summary()

