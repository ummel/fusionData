# Harmonize and Impute Household Housing Costs and Valuation Metrics
#
# PURPOSE & OVERVIEW FOR USERS:
# Standard ACS PUMS housing cost variables contain significant structural missingness
# driven by tenure: renters report gross rent but no property value or property taxes,
# while homeowners report estimated property values, taxes, and mortgage payments
# but no rental value.
#
# To enable consistent economic, wealth, and spatial microsimulation analyses across all
# housing types in fusionACS, this script creates unified, full-universe financial
# variables for every household regardless of tenure:
#   1. 'rentval': Annual rental equivalence (gross rent inclusive of utilities), imputed
#      for homeowners and adjusted via Bureau of Economic Analysis (BEA) owner-premium rules.
#   2. 'propval': Property market value, imputed for renter-occupied dwellings and
#      adjusted via BEA renter-discount rules.
#   3. 'proptax': Annual property tax expenditure, imputed where missing/included in
#      mortgages and enforced to maintain a monotonic relationship with property value.
#   4. 'propins': Annual property insurance expenditure, imputed for renters and uninsured
#      owners, coerced to a monotonic relationship with property value within PUMAs.
#   5. 'mortgage': Annual principal and interest (P&I) mortgage debt payments, isolated
#      from tax/insurance escrows and zeroed for unencumbered owner-occupied properties.
#
# METHODOLOGY & ECONOMIC ADJUSTMENTS:
#   - Isolation of Mortgage Escrows: Removes embedded property tax and homeowners insurance
#     escrow payments from reported monthly mortgage figures ('mrgp', 'smp') to isolate true P&I.
#   - Missing Value Imputation: Employs 'fusionModel::impute()' incorporating MCDC spatial
#     predictors and reference person demographic controls.
#   - BEA Owner Premium & Renter Discount: Follows BEA national accounts methodology
#     (BEA, 2019) to adjust imputed rent values upward for homeowners (reflecting higher
#     average quality/maintenance) and imputed property values downward for rental units.
#   - Spatial Monotonic Constraints: Uses 'fusionModel::monotonic()' to guarantee that
#     imputed tax and insurance obligations increase monotonically with property value
#     within each Public Use Microdata Area (PUMA).
#
# INPUT VARIABLES REQUIRED:
#   - Household-level ('H'): 'hid', 'year', 'state', 'puma*', 'weight', 'ten', 'valp',
#     'grntp', 'mrgp', 'smp', 'mrgt', 'mrgi', 'taxamt', 'insp', 'bld', 'bds'/'bdsp'
#   - Person-level ('P'): Reference person demographics
#   - Spatial Predictors: 'geo-processed/geo_predictors.fst'
#
# OUTPUTS GENERATED:
# Returns a household-level ('H') data frame containing:
#   - 'year', 'hid': Household primary keys
#   - 'rentval', 'propval', 'proptax', 'propins', 'mortgage': Harmonized financial metrics (labeled)

housing <- function(year) {

  cli::cli_alert_info("Executing custom housing financial variables calculation for vintage {year}")

  # Load processed household microdata for the specified vintage
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  if (length(hfile) == 0) stop("Unable to locate processed household microdata for year ", year)

  v <- setdiff(names(fst::fst(hfile)), paste0("rep_", 1:80))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load corresponding person microdata to isolate reference person characteristics
  pfile <- sub("_H_", "_P_", hfile)
  if (!file.exists(pfile)) stop("Unable to locate processed person microdata for year ", year)

  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)
  p <- p[!duplicated(hid)]

  # Calculate annual mortgage payments (P&I) and isolate escrow components
  # Strip out property taxes and homeowners insurance if reported as included in mortgage
  cli::cli_alert_info("Processing mortgage payment escrows, property taxes, and insurance values")
  h <- h %>%
    mutate(
      proptax = ifelse(taxamt < 10, NA, taxamt),
      propins = ifelse(insp < 10, NA, insp),
      mortgage = 12 * (mrgp + smp),
      mortgage = ifelse(mrgt == "Yes, taxes included in payment", mortgage - proptax, mortgage),
      mortgage = ifelse(mrgi == "Yes, insurance included in payment", mortgage - propins, mortgage),
      mortgage = ifelse(ten == "Owned free and clear", 0, mortgage),
      mortgage = ifelse((grepl("Owned with mortgage", ten) & mortgage <= 0) | (mrgp > 0 & mrgp < 10) | (smp > 0 & smp < 10), NA, mortgage),
      mortgage_imp = ifelse(mortgage <= 0, NA, mortgage)
    )

  # Standardize bedroom counts and categorize building structure types
  bedrooms <- h[[intersect(names(h), c('bds', 'bdsp'))]]
  if (is.factor(bedrooms)) bedrooms <- as.integer(bedrooms) - 1L
  bedrooms <- as.integer(cut(bedrooms, breaks = c(-Inf, 1:4, Inf)))

  # Helper function to winsorize top-end bedroom count distributions
  clipFun <- function(x, cumprop = 0.9) {
    x[x == 0] <- 1L
    p <- cumsum(table(x) / length(x))
    i <- max(which(p <= cumprop))
    x[x > i] <- i
    return(x)
  }

  # Clean extreme outliers and stratify structure types for BEA adjustment algorithms
  h <- h %>%
    mutate(owned = grepl("Owned", ten),
           rentval = ifelse(!owned & grntp > 150, 12 * grntp, NA),
           propval = ifelse(owned & valp >= 3000, valp, NA),
           structure = as.character(bld),
           structure = ifelse(grepl("One-family", structure), "Single", structure),
           structure = ifelse(grepl("Mobile", structure) | grepl("Boat", structure), "Mobile", structure),
           structure = ifelse(!structure %in% c("Single", "Mobile"), "Multi", structure),
           bedrooms = !!bedrooms) %>%
    group_by(state) %>%
    mutate(bedrooms = ifelse(structure == "Mobile", 0, clipFun(bedrooms))) %>%
    ungroup()

  # Merge household data with reference person demographics
  d <- h %>%
    collapse::join(p, on = 'hid', verbose = FALSE)

  # Attach MCDC PUMA-level spatial predictor variables
  pvar <- names(select(d, starts_with("puma")))
  if ("puma10" %in% pvar) {
    cli::cli_alert_info("Attaching MCDC spatial predictor variables for imputation")
    mcdc <- fst::read_fst("geo-processed/geo_predictors.fst", as.data.table = TRUE) %>%
      filter(vintage == year) %>%
      select(state, puma10, starts_with("mcdc..")) %>%
      mutate_if(is.numeric, ~ collapse::replace_na(.x, as.integer(median(.x, na.rm = TRUE))))
    d <- d %>%
      collapse::join(mcdc, on = c('state', 'puma10'), verbose = FALSE)
  }

  stopifnot(nrow(d) == nrow(h))

  # Impute missing housing valuations, rents, taxes, insurance, and mortgages across tenure types
  cli::cli_alert_info("Imputing missing tenure-dependent housing parameters via fusionModel")
  d <- fusionModel::impute(d,
                           weight = "weight",
                           ignore = c(pvar, 'hid', 'valp', 'grntp', 'rntp', 'mrgp', 'smp', 'taxamt', 'insp', 'mortgage'),
                           cores = 2)

  # Compute state x structure x bedroom stratum medians for owner/renter valuation adjustments
  cli::cli_alert_info("Applying BEA owner-premium and renter-discount adjustments")
  strata.propval <- d %>%
    filter(owned) %>%
    group_by(state, structure, bedrooms) %>%
    summarize(median_propval = matrixStats::weightedMedian(propval, weight, na.rm = TRUE), .groups = "drop")

  strata.rentval <- d %>%
    filter(!owned) %>%
    group_by(state, structure, bedrooms) %>%
    summarize(median_rentval = matrixStats::weightedMedian(rentval, weight, na.rm = TRUE), .groups = "drop")

  # Apply BEA Owner Premium to owner-occupied imputed rental values
  adj.rent <- d %>%
    left_join(strata.propval, by = c("state", "structure", "bedrooms")) %>%
    mutate(r = ifelse(owned, propval / median_propval, NA),
           adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
           adj = ifelse(owned, adj, 1))

  stopifnot(all(adj.rent$adj[adj.rent$owned] >= 1.05, na.rm = TRUE))

  # Apply BEA Renter Discount to renter-occupied imputed property values
  adj.prop <- d %>%
    left_join(strata.rentval, by = c("state", "structure", "bedrooms")) %>%
    mutate(r = ifelse(owned, NA, median_rentval / rentval),
           adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
           adj = ifelse(owned, 1, 1 / adj))

  stopifnot(all(adj.prop$adj[!adj.prop$owned] <= 1 / 1.05, na.rm = TRUE))

  # Finalize adjusted dollar amounts and enforce minimum floor values
  d <- d %>%
    mutate(rentval = round(rentval * adj.rent$adj),
           propval = round(pmax(3000, propval * adj.prop$adj)),
           proptax = round(pmax(25, proptax * adj.prop$adj)),
           propins = round(pmax(10, propins * adj.prop$adj)),
           mortgage = ifelse(is.na(mortgage), mortgage_imp, mortgage)) %>%
    select(weight, year, hid, state, starts_with("puma"), rentval, propval, proptax, propins, mortgage) %>%
    as.data.table()

  # Enforce monotonic relationship between property values and tax/insurance within PUMAs
  cli::cli_alert_info("Enforcing monotonic relationships for property tax and insurance against property value")
  suppressWarnings(
    d[, `:=`(proptax = round(fusionModel::monotonic(x = propval, y = proptax, w = weight)),
             propins = round(fusionModel::monotonic(x = propval, y = propins, w = weight))),
      by = c('state', pvar)]
  )

  stopifnot(!anyNA(d))

  # Attach descriptive labels to final housing metrics
  d <- d %>%
    select(year, hid, rentval, propval, proptax, propins, mortgage) %>%
    labelled::set_variable_labels(rentval = "Annual rental value of dwelling, including utilities, imputed and adjusted for owner-occupied units",
                                  propval = "Property value reported by owner, imputed and adjusted for renter-occupied units",
                                  proptax = "Typical annual property tax, imputed for renters and coerced to a monotonic relationship with property value within each PUMA",
                                  propins = "Typical annual property insurance, imputed for renters and uninsured owners and coerced to a monotonic relationship with property value within each PUMA",
                                  mortgage = "Annual mortgage payment, principal and interest, zero for households without a mortgage")

  cli::cli_alert_success("Successfully calculated custom housing financial variables for {nrow(d)} households")

  return(d)

}
