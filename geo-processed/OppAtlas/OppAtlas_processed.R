library(tidyverse)
source("R/utils.R")

# Temporary directory to unzip .fsn contents to
td <- tempfile()
dir.create(td)

# Unzip all files in .fsn to temporary directory
zip::unzip(zipfile = "geo-raw/OppAtlas/tract_outcomes.zip", exdir = td)

# Load the extracted .csv file
d <- data.table::fread(list.files(td, full.names = TRUE))

# Selected variables
vars <- c('has_dad', 'jail', 'kfr', 'kir', 'married', 'staycz', 'teenbrth')
# has_dad: Fraction of children who have a male claimer in the year they are linked to parents
# jail: Fraction incarcerated on April 1st, 2010 (where incarceration is defined as residing in a federal detention center, federal prison, state prison, local jail, residential correctional facility, military jail, or juvenile correctional facility)
# kfr: Mean percentile rank (relative to other children born in the same year) in the national distribution of household income (i.e. own earnings and spouse’s earnings) measured as mean earnings in 2014-2015 for the baseline sample
# kir: Mean percentile rank (relative to other children born in the same year) in the national distribution of individual income (i.e. just own earnings) measured as mean earnings in 2014-2015 for the baseline sample
# married: Fraction of children who file their federal income tax return as “married filing jointly” or “married filing separate” in 2015
# staycz: Fraction of children who live in one of their childhood commuting zones in adulthood
# teenbrth: Fraction of women who grew up in the given tract who ever claimed a child who was born when they were between the ages of 13 and 19 as a dependent at any point

# Assemble final result
result <- d %>%
  mutate(state = str_pad(state, 2, pad = 0),
         county10 = str_pad(county, 3, pad = 0),
         tract10 = str_pad(tract, 6, pad = 0),
         vintage = 2015L) %>% # Variables circa 2014-2015
  select(state, county10, tract10, vintage,
         any_of(paste0(vars, "_pooled_pooled_mean")),  # Unconditional outcomes
         any_of(paste0(vars, "_pooled_pooled_p50")),  # Estimated outcomes conditional on national median income of parents
         teenbrth_pooled_female_mean, frac_years_xw_pooled_pooled) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001)

saveRDS(result, "geo-processed/OppAtlas/OppAtlas_processed.rds")
