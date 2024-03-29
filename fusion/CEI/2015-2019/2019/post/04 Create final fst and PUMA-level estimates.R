# This script generates:
# CEI_2015-2019_2019_calib.fst
# CEI_2015-2019_2019_puma.fst

library(fusionData)
library(fusionModel)

#----

# Load calibrated dataset (from .csv.gz)
d <- fread("fusion/CEI/2015-2019/2019/post/CEI_2015-2019_2019_calib.csv.gz")

#----

# Save fst version to disk (NOT using full compression; too slow)
threads_fst(2)  # Can be set higher if sufficient resources available
write_fst(d, path = "fusion/CEI/2015-2019/2019/post/CEI_2015-2019_2019_calib.fst", compress = 75)

#----

# Generate PUMA-level estimates for select variables
# NOTE: Could use mclapply() below for bulk processing given sufficient resources

# NOTE: Only small subset of variables processed currently (for speed)
vars <- c("elec", "gas", "eatout", "airshp")

# ACS static variables
acs <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst", columns = c("weight", "state", "puma10"))

out <- lapply(vars, function(x) {
  analyze(formula = as.formula(paste(x, "~1")),
          implicates = d,
          donor.N = nrow(fst("survey-processed/CEX/CEI/CEI_2015-2019_H_processed.fst")),
          sample_weights = acs$weight,
          static = acs,
          by = c("state", "puma10"))
})

puma <- out %>%
  bind_rows() %>%
  select(state, puma10, response, metric, estimate)

write_fst(puma, path = "fusion/CEI/2015-2019/2019/post/CEI_2015-2019_2019_puma.fst")
