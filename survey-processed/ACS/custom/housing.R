housing <- function(year) {

  # Load household microdata
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  v <- setdiff(names(fst::fst(hfile)), paste0("rep_", 1:80))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load person microdata
  # Retain only the reference person record for each household (used as predictor variables during imputation)
  pfile <- sub("_H_", "_P_", hfile)
  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)
  p <- p[!duplicated(hid)]

  #---

  # # Convert categorical/factor property tax variable prior to 2018 (TAXP) to numeric value (topcoded at $10,000)
  # # See here: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2017.pdf
  # # The factor levels and topcode value used for TAXP did not change from 2005-2017
  # if (year < 2018) {
  #   x <- h$taxp
  #   l <- strsplit(levels(x), " - ")
  #   lwr <- suppressWarnings(as.integer(substring(map(l, 1L), 2)))
  #   upr <- suppressWarnings(as.integer(substring(map(l, 2L), 2)))
  #   val <- ceiling((lwr + upr) / 2)
  #   val[length(val)] <- 10000  # The top-code value prior to 2018
  #   v <- val[as.integer(x)]
  #   v[is.na(v)] <- 0
  #   h$taxamt <- as.integer(v)  # Adds 'taxamt' variable for use in housing-related code below
  #   h$taxp <- NULL
  # }
  #
  # # Convert categorical/factor property value variable prior to 2008 (VAL) to numeric value (topcoded at $1000000)
  # # See here: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict07.pdf
  # # The factor levels and topcode value used for VAL did not change from 2005-2007
  # if (year < 2008) {
  #   x <- h$val
  #   l <- strsplit(levels(x), " - ")
  #   lwr <- suppressWarnings(as.integer(substring(map(l, 1L), 2)))
  #   upr <- suppressWarnings(as.integer(substring(map(l, 2L), 2)))
  #   val <- ceiling((lwr + upr) / 2)
  #   val[2] <- 7500
  #   val[length(val)] <- 1000000  # The top-code value prior to 2008
  #   v <- val[as.integer(x)]
  #   v[is.na(v)] <- 0
  #   h$valp <- as.integer(v)  # Adds 'valp' variable for use in housing-related code below
  #   h$val <- NULL
  # }

  #---

  # Calculate household annual mortgage payment (mortgage) with property taxes and insurance EXCLUDED (i.e. P&I only)
  # Determine if the annual property tax and insurance amounts are valid or need to be imputed (i.e. set to NA)
  # Set property tax (TAXAMT) and home insurance (INSP) to NA if expenditure is included in mortgage payment
  # Total mortgage payment is sum of MRGP (first mortgage) and SMP (all second and junior mortgages and home equity loans)
  # Variables MRGI and MRGT indicate if first mortgage payment includes insurance (MRGI) or property taxes (MRGT)
  # Assume that property taxes can be zero, but insurance payment must be positive if the property is mortgaged
  h <- h %>%
    mutate(
      #proptax = ifelse(taxamt == 0 & mrgt != "No, taxes paid separately or taxes not required" | taxamt == 4, NA, taxamt),
      #propins = ifelse(insp == 0 & mrgi != "No, insurance paid separately or no insurance" | insp == 4, NA, insp),
      #propins = ifelse(insp == 0 & grepl("Owned with mortgage", ten), NA, insp),
      #proptax = ifelse(taxamt == 0 & mrgt == "Yes, taxes included in payment", NA, taxamt),
      #propins = ifelse(insp == 0 & (mrgi == "Yes, insurance included in payment" | grepl("Owned with mortgage", ten)), NA, insp),
      proptax = ifelse(taxamt < 10, NA, taxamt),
      propins = ifelse(insp < 10, NA, insp),
      mortgage = 12 * (mrgp + smp),  # Convert monthly mortgage payments to annual
      mortgage = ifelse(mrgt == "Yes, taxes included in payment", mortgage - proptax, mortgage),
      mortgage = ifelse(mrgi == "Yes, insurance included in payment", mortgage - propins, mortgage),
      mortgage = ifelse(ten == "Owned free and clear", 0, mortgage),
      mortgage = ifelse((grepl("Owned with mortgage", ten) & mortgage <= 0) | (mrgp > 0 & mrgp < 10) | (smp > 0 & smp < 10), NA, mortgage), # Invalid 'mortgage' values; # Sometimes mrgp = 4 or smp = 4 or is present in raw data, but this seems incorrect (set to NA)
      mortgage_imp = ifelse(mortgage <= 0, NA, mortgage)
    )

  #---

  # Create an annual "rental equivalence" (renteq) variable to be imputed for owner-occupied units
  # Rental equivalence is estimated using rental values available for rented units and then applying an "owner premium" post-imputation
  # The owner premium is based on owner's self-reported property values, using the technique described here:
  # https://www.bea.gov/system/files/2019-11/improving-measures-of-national-and-regional-housing-services-us-accounts.pdf
  # RNTP (not used) is the "contract rent"; the rent actually paid by the tenant (possibly including utilities)
  # The GRNTP variable is "gross rent" and includes "estimated utilities" (https://www.census.gov/quickfacts/fact/note/US/HSG860221)
  # From Census: "Gross rent is intended to eliminate differentials that result from varying practices with respect to the inclusion of utilities and fuels as part of the rental payment."
  # Reported GRNTP values <$150/month set to NA and imputed (~0.005 percentile nationally)
  # This means the resulting rental equivalence is the approximate rent inclusive of utilities

  # Convert 'bedrooms' to 5-value integer, regardless of whether original variable is categorical (BDS) or continuous (BDSP)
  bedrooms <- h[[intersect(names(h), c('bds', 'bdsp'))]]
  if (is.factor(bedrooms)) bedrooms <- as.integer(bedrooms) - 1L
  bedrooms <- as.integer(cut(bedrooms, breaks = c(-Inf, 1:4, Inf)))

  # Function to clip/winsorize number of bedrooms
  clipFun <- function(x, cumprop = 0.9) {
    x[x == 0] <- 1L
    p <- cumsum(table(x) / length(x))
    i <- max(which(p <= cumprop))
    x[x > i] <- i
    return(x)
  }

  # Reported GRNTP values <$150/month set to NA and imputed
  # Reported VALP values <$3000 set to NA and imputed
  h <- h %>%
    mutate(owned = grepl("Owned", ten), # Is the housing unit owned?
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

  #---

  # Get PUMA-level MCDC spatial predictor variables
  mcdc <- fst::read_fst("geo-processed/geo_predictors.fst", as.data.table = TRUE) %>%
    filter(vintage == year) %>%
    select(state, puma10, starts_with("mcdc.."))

  # Merge reference person attributes (p) and MCDC spatial predictors (mcdc)
  d <- h %>%
    collapse::join(p, on = 'hid', verbose = FALSE) %>%
    collapse::join(mcdc, on = c('state', 'puma10'), verbose = FALSE)

  stopifnot(nrow(d) == nrow(h))
  rm(p, mcdc)

  # NOTE: Preferable to impute 'mortgage', 'proptax', and 'propins' as a block?
  # Appears to be on unknown bug in fuse() when the number of predictions is small (e.g. 'proptax')
  # check <- which(sapply(d, anyNA))
  d <- fusionModel::impute(d,
                           weight = "weight",
                           ignore = c('hid', 'puma10', 'valp', 'grntp', 'rntp', 'mrgp', 'smp', 'taxamt', 'insp', 'mortgage'),
                           cores = 2)

  #---

  # An "owner premium" is applied to adjust the initial rental equivalence based on imputation of observed contract rents
  # The owner premium is based on owner's self-reported property values, using the BEA technique described here:
  # https://www.bea.gov/system/files/2019-11/improving-measures-of-national-and-regional-housing-services-us-accounts.pdf
  # Page 6: "If the house value is exactly equal to the stratified median value of houses, the owner premium is
  #  15 percent. If the house value is less than the stratified median value, the premium decreases
  #  linearly to a minimum of 5 percent. If the house value is greater than the stratified median value,
  #  the premium increases linearly."

  # Calculate median property value of owner-occupied housing units, by state-structure-bedrooms stratum
  # Used to adjust the rental value of owner-occupied units upward
  strata.propval <- d %>%
    filter(owned) %>% # Restrict to owned housing units
    group_by(state, structure, bedrooms) %>%
    summarize(median_propval = matrixStats::weightedMedian(propval, weight, na.rm = TRUE), .groups = "drop")

  # Used to adjust the property value of renter-occupied units downward
  strata.rentval <- d %>%
    filter(!owned) %>% # Restrict to owned housing units
    group_by(state, structure, bedrooms) %>%
    summarize(median_rentval = matrixStats::weightedMedian(rentval, weight, na.rm = TRUE), .groups = "drop")

  # Applying owner premium for rental value
  adj.rent <- d %>%
    left_join(strata.propval, by = c("state", "structure", "bedrooms")) %>%
    mutate(r = ifelse(owned, propval / median_propval, NA),
           adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
           adj = ifelse(owned, adj, 1))  # If housing unit is not owner, set 'adj' for rental value to 1 (i.e. renter-occupied unit)

  # Confirmation of plausible adjustment values
  stopifnot(all(adj.rent$adj[adj.rent$owned] >= 1.05, na.rm = TRUE))

  # Applying renter discount for property value, property tax, and property insurance
  adj.prop <- d %>%
    left_join(strata.rentval, by = c("state", "structure", "bedrooms")) %>%
    mutate(r = ifelse(owned, NA, median_rentval / rentval),
           adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
           adj = ifelse(owned, 1, 1 / adj))   # If housing unit is owned, set 'adj' to 1 (i.e. owner-occupied unit)

  # Confirmation of plausible adjustment values
  stopifnot(all(adj.prop$adj[!adj.prop$owned] <= 1 / 1.05, na.rm = TRUE))

  d <- d %>%
    mutate(rentval = round(rentval * adj.rent$adj),
           propval = round(pmax(3000, propval * adj.prop$adj)),  # Enforce minimum of $3,000
           proptax = round(pmax(25, proptax * adj.prop$adj)), # Enforce minimum of $25
           propins = round(pmax(10, propins * adj.prop$adj)), # Enforce minimum of $10
           mortgage = ifelse(is.na(mortgage), mortgage_imp, mortgage)) %>%
    select(weight, year, hid, state, puma10, rentval, propval, proptax, propins, mortgage) %>%
    as.data.table()

  #---

  # Enforce monotonic relationship between property value and both property tax and property insurance, by PUMA
  d[, `:=`(proptax = round(fusionModel::monotonic(x = propval, y = proptax, w = weight)),
           propins = round(fusionModel::monotonic(x = propval, y = propins, w = weight))),
    by = c('state', 'puma10')]

  #---

  # Retain and define output variables
  d <- d %>%
    select(year, hid, rentval, propval, proptax, propins, mortgage) %>%
    labelled::set_variable_labels(rentval = "Annual rental value of dwelling, including utilities, imputed and adjusted for owner-occupied units",
                                  propval = "Property value reported by owner, imputed and adjusted for renter-occupied units",
                                  proptax = "Typical annual property tax, imputed for renters and coerced to a monotonic relationship with property value within each PUMA",
                                  propins = "Typical annual property insurance, imputed for renters and uninsured owners and coerced to a monotonic relationship with property value within each PUMA",
                                  mortgage = "Annual mortgage payment, principal and interest, zero for households without a mortgage")

  return(d)

}
