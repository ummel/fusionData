# Compute Household Official Poverty Thresholds, Poverty Ratios, and Imputed Income
#
# PURPOSE & OVERVIEW FOR USERS:
# Measuring household poverty status in survey microdata is critical for welfare policy,
# inequality research, and spatial microsimulation. However, raw survey responses for
# household income ('hincp') at the extreme low end (e.g., zero or near-zero income)
# frequently suffer from underreporting or measurement error, creating implausible
# density spikes near 0% of the Federal Poverty Line (FPL).
#
# This custom script harmonizes household poverty calculations for fusionACS by:
#   1. Matching Official Poverty Thresholds: Attaching the U.S. Census Bureau's official
#      poverty thresholds ('poverty_thresholds' from fusionData) based on total household
#      size ('np'), number of related minor children (<18 years old), and householder
#      senior status (>=65 years old for single/two-person households).
#   2. Smoothing Extreme Low-Income Outliers: Setting reported poverty ratios below 20%
#      (0.20 FPL) to NA, identifying them as potential measurement errors based on
#      empirical density distribution checks.
#   3. Imputing Plausible Poverty Ratios: Leveraging 'fusionModel::impute()' to impute
#      plausible poverty ratios ('pov_ratio') for low-income households based on socio-
#      demographic controls and household characteristics.
#   4. Constructing Imputed Household Income: Back-calculating an alternative household
#      income variable ('hincp_imp' = pov_ratio * pov_thresh) to provide a smoothed,
#      plausible income measure for low-income populations across the fusionACS platform.
#
# INPUT VARIABLES REQUIRED:
#   - Household-level ('H'): 'hid', 'year', 'np' (number of persons), 'hincp', 'fincp', 'weight'
#   - Person-level ('P'): 'hid', 'agep' (age)
#   - Package Data: 'fusionData::poverty_thresholds' (pre-processed Census poverty matrices)
#
# OUTPUTS GENERATED:
# Returns a household-level ('H') data frame containing:
#   - 'year', 'hid': Household primary keys
#   - 'pov_thresh': Official Census poverty threshold dollar amount (labeled)
#   - 'pov_ratio': Income-to-poverty threshold ratio, imputed below 0.20 FPL (labeled)
#   - 'hincp_imp': Imputed/smoothed annual household income in dollars (labeled)

poverty <- function(year) {

  cli::cli_alert_info("Executing custom poverty variables calculation for vintage {year}")

  # Load pre-processed Census Bureau official poverty threshold lookup matrix from fusionData
  data(poverty_thresholds, package = "fusionData")

  # Load processed household microdata for the specified vintage
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  if (length(hfile) == 0) stop("Unable to locate processed household microdata for year ", year)

  v <- setdiff(names(fst::fst(hfile)), c('puma10', paste0("rep_", 1:80)))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load person microdata to calculate household composition keys (minors and senior householder)
  pfile <- sub("_H_", "_P_", hfile)
  if (!file.exists(pfile)) stop("Unable to locate processed person microdata for year ", year)

  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)

  # Calculate number of minors and senior householder status per household
  p2 <- p[, list(minors = sum(agep < 18), senior = agep[1] >= 65), by = "hid"]
  p <- p[!duplicated(hid)]  # Retain reference person record for demographic imputation predictors

  cli::cli_alert_info("Matching official poverty thresholds and calculating income-to-poverty ratios")

  # Consolidate household and person characteristics, align keys to match Census threshold matrices,
  # flag extreme underreported incomes (<0.20 FPL), and impute realistic poverty ratios
  out <- h %>%
    merge(p, by = 'hid', sort = FALSE) %>%
    merge(p2, by = 'hid', sort = FALSE) %>%
    mutate(size = pmin(np, 9L),
           minors = pmin(minors, 8L),
           minors = ifelse(minors == size, size - 1L, minors)) %>%  # Prevent edge case where all household members are minors
    merge(poverty_thresholds, by = key(poverty_thresholds), all.x = TRUE, sort = FALSE) %>%
    mutate(pov_ratio = signif(hincp / threshold, 4),
           pov_ratio = ifelse(pov_ratio < 0.2, NA, pov_ratio))  # Set <0.20 FPL to NA to trigger imputation for severe underreporting

  cli::cli_alert_info("Imputing poverty ratios for low-income underreporting households via fusionModel")

  out <- out %>%
    fusionModel::impute(weight = "weight", ignore = c('hid', 'hincp', 'fincp'), cores = 2) %>%
    mutate(pov_thresh = threshold,
           hincp_imp = round(ifelse(hincp / threshold < 0.2, pov_ratio * pov_thresh, hincp))) %>%

    # Select final custom variables and attach metadata descriptions
    select(year, hid, pov_thresh, pov_ratio, hincp_imp) %>%
    labelled::set_variable_labels(pov_thresh = "Household poverty threshold based on household size, number of children, and age of householder (dollars)",
                                  pov_ratio = "Ratio of household income to poverty threshold, imputed for households with original income below 20% of FPL",
                                  hincp_imp = "Household income in the past 12 months, imputed for households with reported income below 20% of FPL. Calculated as pov_ratio * pov_thresh.")

  cli::cli_alert_success("Successfully calculated custom poverty variables for {nrow(out)} households")

  return(out)
}
