# Function to add household-level official poverty thresholds and ratio of household income to the threshold
# Poverty ratio is imputed for households with an initial reported poverty threshold below 20% of FPL
# An alternative/imputed household income variable is then constructed as pov_ratio * pov_thresh
# Useful for identifying plausible poverty status of ACS households
# The official poverty threshold values are pre-processed (see: data-raw/poverty_thresholds.R)

poverty <- function(year) {

  data(poverty_thresholds, package = "fusionData")

  # Load household microdata
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  v <- setdiff(names(fst::fst(hfile)), c('puma10', paste0("rep_", 1:80)))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load person microdata
  pfile <- sub("_H_", "_P_", hfile)
  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)
  p2 <- p[, list(minors = sum(agep < 18), senior = agep[1] >= 65), by = "hid"]
  p <- p[!duplicated(hid)]  # Retain the reference person record for each household (used as predictor variables during imputation)

  # Compute desired custom variables
  # Set original pov_ratio to NA if below 0.2 so it is subsequently imputed
  # Selected 0.2 value by looking at smoothness of distribution at low poverty ratios: plot(density(out$pov_ratio[out$pov_ratio < 1]))
  out <- h %>%
    merge(p, by = 'hid', sort = FALSE) %>%
    merge(p2, by = 'hid', sort = FALSE) %>%
    mutate(size = pmin(np, 9L),
           minors = pmin(minors, 8L),
           minors = ifelse(minors == size, size - 1L, minors)) %>%  # Handle rare cases of all minors in a household
    merge(poverty_thresholds, by = key(poverty_thresholds), all.x = TRUE, sort = FALSE) %>%
    mutate(pov_ratio = signif(hincp / threshold, 4),
           pov_ratio = ifelse(pov_ratio < 0.2, NA, pov_ratio)) %>%
    fusionModel::impute(weight = "weight", ignore = c('hid', 'hincp', 'fincp'), cores = 2) %>%
    mutate(pov_thresh = threshold,
           hincp_imp = round(ifelse(hincp / threshold < 0.2, pov_ratio * pov_thresh, hincp))) %>%

    # Retain and define output variables
    select(year, hid, pov_thresh, pov_ratio, hincp_imp) %>%
    labelled::set_variable_labels(pov_thresh = "Household poverty threshold based on household size, number of children, and age of householder (dollars)",
                                  pov_ratio = "Ratio of household income to poverty threshold, imputed for households with original income below 20% of FPL",
                                  hincp_imp = "Household income in the past 12 months, imputed for households with reported income below 20% of FPL. Calculated as pov_ratio * pov_thresh.")

  return(out)
}
