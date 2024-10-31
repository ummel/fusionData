library(tidyverse)
library(data.table)
source("R/utils.R")

# FUNCTION INPUTS
# Specify file paths to dictionary and raw zip data files
# year <- 2015
# respondent <- "P"

processACSmicrodata <- function(year, respondent) {

  # Check input arguments
  stopifnot({
    year %% 1 == 0 & year >= 2005
    toupper(respondent) %in% c("H", "P")
  })

  # Processing household data?
  hus <- toupper(respondent) == "H"

  # Report which data is being processed
  cat("Processing ", year, " ACS-PUMS ", ifelse(hus, "Household", "Person"), "-level microdata\n", sep = "")

  # Files in the associated /survey-raw directory
  raw.files <- list.files(file.path("survey-raw/ACS", year), full.names = TRUE)

  # Path to data dictionary file (.txt or .csv)
  dictionary.file <- grep("Dict", raw.files, fixed = TRUE, value = TRUE)
  if (length(dictionary.file) != 1) stop("Could not locate dictionary file")

  # Path to "hus" or "pus" raw data (zipped csv file)
  # Link for file download: https://www2.census.gov/programs-surveys/acs/data/pums/
  data.zipfile <- if (hus) {
    grep("hus.zip", raw.files, fixed = TRUE, value = TRUE)
  } else {
    grep("pus.zip", raw.files, fixed = TRUE, value = TRUE)
  }
  if (length(data.zipfile) != 1) stop("Could not locate microdata file")

  #-----

  # Process codebook into standard format
  cat("Processing raw codebook data into useful format\n")
  source("survey-processed/ACS/processACScodebook.R")
  codebook <- processACScodebook(dictionary.file)

  #---------------------------------------

  # Primary weights variable
  wvar <- ifelse(hus, "WGTP", "PWGTP")

  # Unzip raw .zip file
  cat("Un-zipping raw microdata files to temporary directory\n")
  tdir <- tempfile()
  unzip(data.zipfile, exdir = tdir, overwrite = TRUE)
  dfiles <- list.files(path = tdir, pattern = ".csv$", full.names = TRUE)

  # Read PUMS .csv data files
  cat("Reading raw microdata from disk\n")
  d <- dfiles %>%
    #map_dfr(data.table::fread, colClasses = c(SERIALNO = 'character')) %>%
    map_dfr(data.table::fread) %>%
    rename_with(toupper)   # Ensure upper-case names for consistency for 'codebook'; replicate weights are sometimes lower-case in the raw data
  #mutate(SERIALNO_original = SERIALNO)   # Retain copy of original SERIALNO for row-ordering at end

  # Delete temporary files
  unlink(tdir, recursive = TRUE)

  # Replace literal empty strings ("") with NA for character type columns
  # fread() does not convert empty strings to NA, as they are ambiguous
  for (i in 1:ncol(d)) {
    x <- d[[i]]
    if (is.character(x)) set(d, j = i, value = na_if(x, ""))
  }

  # Function to convert PUMS SERIALNO to a standardized 32-bit integer identifier
  # x: Native/raw ACS PUMS Housing unit/Group quarter identifier (SERIALNO)
  # hu: Logical vector indicating TRUE if x/SERIALNO references a housing unit (as opposed to a group quarter)
  cleanACSID <- function(x, hu) {
    n <- data.table::uniqueN(x)
    stopifnot(is.logical(hu) & length(hu) == length(x))
    x <- str_sub(x, start = -9)
    x <- str_pad(x, width = 9, pad = 0)
    substring(x, 1, 2) <- ifelse(hu, "01", "02")  # Assigns 01 in front for HU and 02 for GQ records
    x <- as.integer(x)  # Convert to integer
    stopifnot(n == data.table::uniqueN(x) & !anyNA(x)) # Safety check on number of unique ID's
    return(x)
  }

  # Create standardized housing unit ID (hid) via cleanACSID()
  gq <- filter(codebook, grepl("group quarters population", label))
  hu <- if (hus) d$WGTP > 0 else !d[[gq$var[1]]] %in% gq$value
  d[, SERIALNO := cleanACSID(SERIALNO, hu)]

  #-------------

  # Manual check against household-level processed data
  # h <- fst::read_fst("survey-processed/ACS/2017/ACS_2017_H_processed.fst")
  # uniqueN(h$hid)
  # uniqueN(d$SERIALNO[hu])
  # d[, CHECK := cleanACSID(SERIALNO, hu)]
  # uniqueN(d$CHECK)
  # uniqueN(d$CHECK[hu])
  # all(h$hid %in% d$CHECK) # This needs to be true

  #-------------

  # Apply 'ADJHSG' and 'ADJINC' adjustment to appropriate variables
  # Note that this code makes no adjustments prior to 2008, as there was only a single ADJUST variable for dollar amounts
  v.adjhsg <- filter(codebook, var %in% names(d) & adj == "ADJHSG")$var
  v.adjinc <- filter(codebook, var %in% names(d) & adj == "ADJINC")$var
  d <- d %>%
    mutate_at(v.adjhsg, ~ round(.x * (ADJHSG / 1e6))) %>%
    mutate_at(v.adjinc, ~ round(.x * (ADJINC / 1e6)))

  # If 'hus', remove group quarter observations AND vacant housing units
  # Note that person records retain individuals in group quarters, while household records have GQ's removed because they have NA's for many variables
  # Remove any variables lacking variation (this drops ADJHSG and ADJINC)
  # Ensure observations restricted to U.S. states and D.C.
  if (hus) d <- d[WGTP > 0 & NP > 0, ]
  d <- d %>%
    select_if(~ novary(.x) == FALSE) %>%
    filter(ST %in% 1:56)

  # Safety check: All processed household 'hid' values must be present in the person-level processed SERIALNO
  if (!hus) {
    h <- fst::read_fst(paste0("survey-processed/ACS/", year, "/ACS_", year, "_H_processed.fst"), columns = "hid")
    if (!all(h$hid %in% d$SERIALNO)) stop("Not all of the household data 'hid' values are present in the processed person-level SERIALNO values")
    rm(h)
  }

  # Adjusting codebook for consistency with the data/variables in 'd'
  cat("Making codebook consistent with the microdata\n")
  codebook <- codebook %>%
    filter(var %in% names(d)) %>%
    add_count(var) %>%
    filter(!(n > 1 & is.na(value) & var %in% names(which(!map_lgl(d, anyNA)))))

  # This conversion works OK for 'hus' but not for 'pus' variable descriptions
  if (hus) codebook <- mutate(codebook, desc = ifelse(custom_desc, desc, str_to_sentence(desc)))

  #---------------------------------------

  # Check for possible issues in 'codebook'
  # These should be investigated manually in 'codebook' and 'd' and any corrections to codebook made below OR edits introduced in 'processACScodebook.R'
  issues <- filter(codebook, is.na(value), label == "")
  if (nrow(issues) > 0) {
    cat("Identified potential issues in 'codebook':\n")
    print(issues)
    cat("These cases will have 'label' set to NA to force imputation.\nIf this is not correct, please STOP and correct the codebook processing code\n")
  }

  # Can insert additional, manual codebook fix-ups if necessary
  # By default, this simply sets missing 'label' entries to NA to force imputation later
  codebook <- codebook %>%
    mutate(label = ifelse(is.na(value) & label == "", NA, label)) # This sets any remaining blank labels to NA (to be imputed later)

  #---------------------------------------

  # Assign ACS labels from codebook to the data
  # Update the values in 'd' with labels from the codebook (i.e. replace the original integer values with text)
  cat("Assigning codebook labels to the microdata\n")

  # Specify which variables should be treated as ordered factors
  # In general, we want to coerce unordered factors to ordered factors whenever feasible (there is some judgement involved)
  # Note that which variables are ordered factors can potentially change over time
  of.hus <- c('ACR','AGS','BDS','MV','R18','R60','R65','RMS','VEH','WIF','YBL','YRBLT')
  of.pus <- c('CITWP','DECADE','DRAT','DRIVESP','ENG','GCM','JWAP','JWDP','JWRIP','MARHYP','MARHT','QTRBIR','SCHG','SCHL','SFN','WKL','YOEP')
  ordered.factors <- c(of.hus, of.pus)

  # Ordered factor variables present in the data
  ordfac <- intersect(ordered.factors, codebook$var)
  if (length(ordfac)) cat(" -- Treating the following variables as ordered factors:\n", paste(ordfac, collapse = "\n"), "\n", sep = "")

  # Only retain variables remaining in the codebook
  dvars <- c(intersect(names(d), codebook$var))
  d <- d[, ..dvars]

  # Update variable values with associated labels from 'codebook'
  # Loop through each variable in 'd', assigning labels when applicable
  cat("Assigning labels to raw data\n")
  pb <- txtProgressBar(max = length(dvars), style = 3)
  for (i in seq_along(dvars)) {

    v <- dvars[i]
    cb <- filter(codebook, var == v)
    x <- d[[v]]
    y <- unlist(cb$value)
    z <- unlist(cb$label)

    # Update 'x' with new value labels, if necessary
    if (any(!is.na(y)) | any(!is.na(z))) {
      m <- match(x, y)
      new.labels <- z[na.omit(m)]
      x[!is.na(m)] <- new.labels
    }

    # Coerce result to ordered factor, if specified
    # Note that levels are restricted to those actually present in the data
    if (v %in% ordfac) {
      num.na <- sum(is.na(x))
      x <- factor(x, levels = intersect(z, x), ordered = TRUE)
      # This is a final safety check to ensure no NA's introduced inadvertently
      if (sum(is.na(x)) != num.na) cat(v, ": introduced", sum(is.na(x)), "NA values (subsequently imputed) due to values in raw data not being present in codebook. Should be reported to survey administrators.\n")
    }

    # Convert variable type; leaves ordered factors unchanged
    if (is.double(x)) x <- convertInteger(x, threshold = 1)
    if (is.character(x)) x <- type.convert(x, as.is = FALSE)

    # Ensure unordered factor levels are sorted according to codebook order of levels
    # There is often useful information in the codebook ordering of the factor levels
    # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
    if (is.factor(x) & !is.ordered(x)) {
      num.na <- sum(is.na(x))
      if (all(x %in% cb$label)) {
        x <- factor(x, levels = intersect(cb$label, unique(x)))
      } else {
        x <- factor(x, levels = sort(unique(x)))
      }
      stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }

    # Update column in data.table 'd', by reference
    set(d, j = v, value = x)

    # Update progress bar
    setTxtProgressBar(pb, i)

  }
  close(pb)

  #-----

  # Convert "ST" and "PUMA" to factors; they are coerced to integers by type.convert() above
  # This ensures that "ST" is treated as factor in imputation step
  d[, ST := factor(str_pad(ST, width = 2, pad = 0))]
  d[, PUMA := factor(str_pad(PUMA, width = 5, pad = 0))]

  #---------------------------------------

  if (hus) {

    # Adjust and modify housing-related variables
    cat("Adjusting housing variables\n")
    #setDT(d)

    # Convert categorical/factor property tax variable prior to 2018 (TAXP) to numeric value (topcoded at $10,000)
    # See here: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2017.pdf
    # The factor levels and topcode value used for TAXP did not change from 2005-2017
    if (year < 2018) {
      x <- d$TAXP
      l <- strsplit(levels(x), " - ")
      lwr <- suppressWarnings(as.integer(substring(map(l, 1L), 2)))
      upr <- suppressWarnings(as.integer(substring(map(l, 2L), 2)))
      val <- ceiling((lwr + upr) / 2)
      val[length(val)] <- 10000  # The top-code value prior to 2018
      v <- val[as.integer(x)]
      v[is.na(v)] <- 0
      set(d, j = "TAXAMT", value = as.integer(v))
      set(d, j = "TAXP", value = NULL)
      codebook$var <- replace(codebook$var, codebook$var == "TAXP", "TAXAMT")
      #d$TAXP <- d$TAXAMT <- as.integer(v)  # Adds temporary 'TAXAMT' variable for use in housing-related code below
    }

    # Convert categorical/factor property value variable prior to 2008 (VAL) to numeric value (topcoded at $1000000)
    # See here: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict07.pdf
    # The factor levels and topcode value used for VAL did not change from 2005-2007
    if (year < 2008) {
      x <- d$VAL
      l <- strsplit(levels(x), " - ")
      lwr <- suppressWarnings(as.integer(substring(map(l, 1L), 2)))
      upr <- suppressWarnings(as.integer(substring(map(l, 2L), 2)))
      val <- ceiling((lwr + upr) / 2)
      val[2] <- 7500
      val[length(val)] <- 1000000  # The top-code value prior to 2008
      v <- val[as.integer(x)]
      v[is.na(v)] <- 0
      set(d, j = "VALP", value = as.integer(v))
      set(d, j = "VAL", value = NULL)
      codebook$var <- replace(codebook$var, codebook$var == "VAL", "VALP")
      #d$VAL <- d$VALP <- as.integer(v)  # Adds temporary 'VALP' variable for use in housing-related code below
    }

    # If property tax (TAXAMT) or home insurance (INSP) is zero, but included in mortgage payment, set to NA so it is imputed
    # Or, if there is a mortgage present, assume home insurance most be non-zero
    d <- d %>%
      mutate(
        TAXAMT = ifelse(TAXAMT == 0 & MRGT == "Yes, taxes included in payment", NA, TAXAMT),
        INSP = ifelse(INSP == 0 & (MRGI == "Yes, insurance included in payment" | grepl("Owned with mortgage", TEN)), NA, INSP)
      )

  }

  #---------------------------------------

  # NOT USED: MOVE ELSEWHERE
  # if (hus) {
  #
  #   cat("Creating custom mortgage payment variable\n")
  #
  #   # Calculate household annual mortgage payment (mortgage) with property taxes and insurance excluded
  #   # Determine if the annual property tax and insurance amounts are valid or need to be imputed (i.e. set to NA)
  #   # Set property tax (TAXAMT) and home insurance (INSP) to NA if expenditure is included in mortgage payment
  #   # Total mortgage payment is sum of MRGP (first mortgage) and SMP (all second and junior mortgages and home equity loans)
  #   # Variables MRGI and MRGT indicate if first mortgage payment includes insurance (MRGI) or property taxes (MRGT)
  #   # Assume that property taxes can be zero, but insurance payment must be positive if the property is mortgaged
  #   d <- d %>%
  #     mutate(
  #       mortgage = 12 * (MRGP + SMP),  # Convert monthly mortgage payments to annual
  #       mortgage = ifelse((MRGP > 0 & MRGP < 10) | (SMP > 0 & SMP < 10), NA, mortgage), # Sometimes MRGP = 4 or SMP = 4 is present in raw data, but this seems incorrect (set NA to impute)
  #       mortgage = ifelse(MRGT == "Yes, taxes included in payment", mortgage - TAXAMT, mortgage),
  #       mortgage = ifelse(MRGI == "Yes, insurance included in payment", mortgage - INSP, mortgage),
  #       mortgage = ifelse(TEN == "Owned free and clear", 0, mortgage),
  #       mortgage = ifelse(grepl("Owned with mortgage", TEN) & mortgage <= 0, NA, mortgage), # Any remaining invalid 'mortgage' values
  #       TAXAMT = ifelse(TAXAMT == 0 & MRGT == "Yes, taxes included in payment", NA, TAXAMT),
  #       INSP = ifelse(INSP == 0 & (MRGI == "Yes, insurance included in payment" | grepl("Owned with mortgage", TEN)), NA, INSP)
  #     )
  #
  # }

  #---------------------------------------

  # NOT USED: MOVE ELSEWHERE
  # if (hus) {
  #
  #   # Create annual "rental value" (rentval) variable to be imputed for owner-occupied units
  #   # Create "property value" (propval) variable to be imputed for renter-occupied units
  #   cat("Creating custom rental value and property value variables\n")
  #
  #   # Rental equivalence is estimated using rental values available for rented units and then applying an "owner premium" post-imputation (code below)
  #   # The owner premium is based on owner's self-reported property values, using the technique described here:
  #   # https://www.bea.gov/system/files/2019-11/improving-measures-of-national-and-regional-housing-services-us-accounts.pdf
  #   # RNTP (not used) is the "contract rent"; the rent actually paid by the tenant (possibly including utilities)
  #   # The GRNTP variable is "gross rent" and includes "estimated utilities" (https://www.census.gov/quickfacts/fact/note/US/HSG860221)
  #   # From Census: "Gross rent is intended to eliminate differentials that result from varying practices with respect to the inclusion of utilities and fuels as part of the rental payment."
  #   # Reported GRNTP values <$150/month set to NA and imputed (~0.005 percentile nationally)
  #   # Reported VALP values <$1000 set to NA and imputed (~0.005 percentile nationally for mobile homes)
  #   # NOTE: This means the resulting rental equivalence is the approximate rent inclusive of utilities
  #   d <- d %>%
  #     mutate(rentval = ifelse(grepl("Rented", TEN) & GRNTP >= 150, 12 * GRNTP, NA),
  #            propval = ifelse(grepl("Owned", TEN) & VALP >= 1000, VALP, NA))
  #
  #   # If necessary, remove temporary TAXAMT and VALP variables
  #   if (year < 2018) d$TAXAMT <- NULL
  #   if (year < 2008) d$VALP <- NULL
  #
  # }

  #---------------------------------------

  if (hus) {

    # If prior to survey year 2018, add utility fuel cost flag variables to microdata
    # Prior to 2018, the utility expenditure variables (e.g. ELEP) included de facto categorical information about payment status as an integer entry
    # For example: "1" = "Included in rent or in condo fee"
    # Starting in 2018, Census Bureau assigns this information to separate "cost flag variables" that are easier to work with
    # The function utilityCostFlags() adds the cost flag variables for pre-2018 ACS vintages and revises the original expenditure variables as necessary
    # This creates consistent utility expenditure variables across years

    if (year < 2018) {

      cat("Adding utility cost flag variables\n")
      stopifnot(all(c("ELEP", "FULP", "GASP", "WATP") %in% names(d)))

      d$ELEFP <- c("Included in rent or in condo fee", "No charge or electricity not used")[d$ELEP]
      d$ELEFP <- factor(replace_na(d$ELEFP, replace = "Valid monthly electricity cost in ELEP"))
      d$ELEP[d$ELEP < 3] <- 0L

      d$FULFP <- c("Included in rent or in condo fee", "No charge or fuel other than gas or electricity not used")[d$FULP]
      d$FULFP <- factor(replace_na(d$FULFP, replace = "Valid annual fuel cost in FULP"))
      d$FULP[d$FULP < 3] <- 0L

      d$GASFP <- c("Included in rent or in condo fee", "Included in electricity payment", "No charge or gas not used")[d$GASP]
      d$GASFP <- factor(replace_na(d$GASFP, replace = "Valid monthly gas cost in GASP"))
      d$GASP[d$GASP < 4] <- 0L

      d$WATFP <- c("Included in rent or in condo fee", "No charge")[d$WATP]
      d$WATFP <- factor(replace_na(d$WATFP, replace = "Valid annual water cost in WATP"))
      d$WATP[d$WATP < 3] <- 0L

    }

  }

  #---------------------------------------

  # Impute any remaining missing values in 'd'

  if (anyNA(d)) {

    # Which variables have missing values and how frequent are they?
    na.count <- colSums(is.na(d))
    na.count <- na.count[na.count > 0]
    na.count <- na.count / nrow(d)  # Proportion of values that are missing
    cat("Percentage of missing values:\n")
    print(round(na.count * 100, 2))

    # # Get PUMA-level MCDC spatial predictor variables
    # mcdc <- fst::read_fst("geo-processed/geo_predictors.fst") %>%
    #   filter(vintage == year) %>%
    #   select(state, puma10, starts_with("mcdc..")) %>%
    #   rename(ST = state, PUMA = puma10)
    #
    # # Merge MCDC spatial predictors
    # d <- left_join(d, mcdc, by = join_by(ST, PUMA))

    cat("Imputing missing values\n")

    # ignore <- setdiff(names(d), c(wvar, names(mcdc), "DIVISION", "REGION", "NP", "ACR", "BLD", "FS", "HFL", "HHL", "HHT", "HUPAC", "NOC", "BDSP", "BDS", "RMSP", "RMS", "TEN", "VEH", "YBL", "YRBLT", "HINCP", "FES", "WIF", "R18", "R65", "HHLDRAGEP", "HHLDRRAC1P"))
    # ignore <- c(ignore, "PUMA")  # Ignore PUMA factor variable
    # rm(mcdc)

    # Use impute() to impute missing values in 'd'
    ignore <- names(select(d, SERIALNO, PUMA, starts_with("WGTP"), starts_with("PWGTP")))
    d <- fusionModel::impute(d, weight = wvar, ignore = ignore)

    # Remove MCDC variables
    #d <- select(d, -starts_with("mcdc.."))

  }

  #---------------------------------------

  # NOT USED: MOVE ELSEWHERE
  # if (hus) {
  #
  #   # After imputation is complete:
  #
  #   # Update the MRGP and SMP variables so they correctly sum to 'mortgage'
  #   # This is necessary in case some of the 'mortgage' values were imputed above
  #   # d <- d %>%
  #   #   mutate(SMP = round(mortgage * smp_share),
  #   #          MRGP = mortgage - SMP)
  #
  #   # Apply owner premium for imputed rental values
  #   # Apply renter discount for imputed property values
  #   cat("Adjusting imputed rental and property values\n")
  #
  #   # The function below applies an "owner premium" adjustment to initial rental equivalence based on imputation of observed contract rents
  #   # The owner premium is based on owner's self-reported property values, using the BEA technique described here:
  #   # https://www.bea.gov/system/files/2019-11/improving-measures-of-national-and-regional-housing-services-us-accounts.pdf
  #   # Page 6: "If the house value is exactly equal to the stratified median value of houses, the owner premium is
  #   #  15 percent. If the house value is less than the stratified median value, the premium decreases
  #   #  linearly to a minimum of 5 percent. If the house value is greater than the stratified median value,
  #   #  the premium increases linearly."
  #
  #   # Post-imputation input variables
  #   rentval <- d$rentval
  #   propval <- d$propval
  #   tenure <- d$TEN
  #   state <- d$ST
  #   structure <- d$BLD
  #   bedrooms <- if (year < 2008) d$BDS else d$BDSP  # Use 'BDS" factor variable prior to 2008
  #   weight <- d$WGTP
  #
  #   # Function to clip/winsorize number of bedrooms
  #   clipFun <- function(x, cumprop = 0.9) {
  #     p <- cumsum(table(x) / length(x))
  #     i <- max(which(p <= cumprop))
  #     x[x > i] <- i
  #     return(x)
  #   }
  #
  #   # Convert 'bedrooms' to 5-value integer, regardless of whether original variable is categorical (BDS) or continuous (BDSP)
  #   if (is.factor(bedrooms)) bedrooms <- as.integer(bedrooms) - 1
  #   bedrooms <- as.integer(cut(bedrooms, breaks = c(-Inf, 1:4, Inf)))
  #
  #   # Prepare input data set; create state-level "Other" category for infrequent 'structure' and 'bedrooms' values
  #   # All mobile home units are assigned to a single stratum, regardless of number of bedrooms
  #   dset <- data.frame(rentval, propval, tenure, state, structure, bedrooms, weight) %>%
  #     mutate(owned = grepl("Owned", tenure), # Is the housing unit owned?
  #            structure = as.character(structure),
  #            structure = ifelse(grepl("One-family", structure), "Single", structure),
  #            structure = ifelse(grepl("Mobile", structure) | grepl("Boat", structure), "Mobile", structure),
  #            structure = ifelse(!structure %in% c("Single", "Mobile"), "Multi", structure)) %>%
  #     group_by(state) %>%
  #     mutate(bedrooms = ifelse(structure == "Mobile", 1L, clipFun(bedrooms))) %>%
  #     ungroup()
  #
  #   # Safety check on row ordering
  #   stopifnot(all.equal(rentval, dset$rentval))
  #
  #   # Calculate median property value of owner-occupied housing units, by state-structure-bedrooms stratum
  #   # Used to adjust the rental value of owner-occupied units upward
  #   strata.propval <- dset %>%
  #     filter(owned) %>% # Restrict to owned housing units
  #     group_by(state, structure, bedrooms) %>%
  #     summarize(median_propval = matrixStats::weightedMedian(propval, weight), .groups = "drop")
  #
  #   # Used to adjust the property value of renter-occupied units downward
  #   strata.rentval <- dset %>%
  #     filter(!owned) %>% # Restrict to owned housing units
  #     group_by(state, structure, bedrooms) %>%
  #     summarize(median_rentval = matrixStats::weightedMedian(rentval, weight), .groups = "drop")
  #
  #   #---
  #
  #   cat(" -- Applying owner premium for imputed rental value\n")
  #   adj.rentval <- dset %>%
  #     left_join(strata.propval, by = c("state", "structure", "bedrooms")) %>%
  #     mutate(r = ifelse(owned, propval / median_propval, NA),
  #            adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
  #            adj = ifelse(owned, adj, 1),  # If housing unit is not owner, set 'adj' for rental value to 1 (i.e. renter-occupied unit)
  #            rentval = rentval * adj)
  #
  #   # Update 'rentval' with adjusted values
  #   d$rentval <- round(adj.rentval$rentval)
  #
  #   # Confirmation of plausible adjustment values
  #   #summary(adj.rentval$adj[adj.rentval$owned])
  #
  #   #---
  #
  #   cat(" -- Applying renter discount for imputed property value\n")
  #   adj.propval <- dset %>%
  #     left_join(strata.rentval, by = c("state", "structure", "bedrooms")) %>%
  #     mutate(r = ifelse(owned, NA, median_rentval / rentval),
  #            adj = ifelse(r <= 0.5, 1.05, ifelse(r > 1, 1 + 0.15 + 0.3 * (r - 1), 1 + 0.05 + 0.2 * (r - 0.5))),
  #            adj = ifelse(owned, 1, 1 / adj),   # If housing unit is owned, set 'adj' to 1 (i.e. owner-occupied unit)
  #            propval = pmax(1000, propval * adj))  # Enforce minimum of $1,000
  #
  #   # Update 'propval' with adjusted values
  #   d$propval <- round(adj.propval$propval)
  #
  #   # Confirmation of plausible adjustment values
  #   #summary(adj.propval$adj[!adj.propval$owned])
  #
  # }

  #---------------------------------------

  # Assemble output
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost in the process
  cat("Assembling final output\n")
  d <- d %>%
    arrange(SERIALNO) %>%
    mutate_if(is.factor, safeCharacters) %>%
    mutate_if(is.numeric, convertInteger, threshold = 1) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%
    rename(hid = SERIALNO, weight = !!wvar, state = ST) %>%
    rename_with(~ gsub(wvar, "REP_", .x, fixed = TRUE), .cols = starts_with(wvar)) %>%  # Rename replicate weight columns to standardized names
    rename_with(tolower) %>%
    mutate(year = as.integer(year))

  # Rename the generic "puma" variable to indicate the census geography vintage
  # https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.2020.html
  # 2020 census geography was used starting in 2022
  # 2010 census geography was used starting in 2012
  # 2000 census geography used prior to 2012
  pvar <- ifelse(year >= 2022, "puma20", ifelse(year >= 2012, "puma10", "puma00"))
  i <- which(names(d) == "puma")
  names(d)[i] <- pvar

  # For person records, add 'pid' variable identifying each person within household (reference person = 1)
  if (!hus) {
    cat("Adding person identifier (pid) variable\n")
    rvar <- tolower(filter(codebook, label == "Reference person")$var)  # Name of the household member relationship variable
    d <- addPID(data = d, hid = "hid", refvar = rvar)
  }

  #---------------------------------------

  # Add manual/custom variable definitions/labels for modified, undefined, or ambiguous variables
  cat("Assigning custom variable definitions\n")

  # Manual variable definitions; these need not be present in the data (silently ignored if not present)
  manual.defs <- list(
    year = "Survey year",
    hid = "Housing unit ID constructed from original PUMS SERIALNO",
    weight = ifelse(hus, "Housing unit central sampling weight", "Person central sampling weight"),
    state = "State FIPS code",
    puma00 = "Public use microdata area code based on 2000 census definition",
    puma10 = "Public use microdata area code based on 2010 census definition",
    puma20 = "Public use microdata area code based on 2020 census definition",
    # mortgage = "Annual mortgage payment, principal and interest",
    # rentval = "Annual rental value including utilities, imputed for owner-occupied units",
    # propval = "Property value reported by owner, imputed for renter-occupied units",
    elefp = "Electricity cost flag variable",
    fulfp = "Fuel cost flag variable",
    gasfp = "Gas cost flag variable",
    watfp = "Water cost flag variable"
  )

  # Assign the manual variable labels/definitions
  for (v in names(manual.defs)) {
    if (v %in% names(d)) {
      labelled::var_label(d[[v]]) <- manual.defs[[v]]
    }
  }

  # Identify which variables have definitions/labels and prepare to drop those that are not defined
  vlabs <- labelled::var_label(d)
  vkeep <- names(which(lengths(vlabs) > 0))
  drop <- setdiff(names(d), vkeep)  # Variables to be dropped due to absence of variable description (TO DO: Print to console?)
  if (length(drop)) cat("Removing the following undefined variable(s):\n", paste(drop, collapse = "\n"), "\n", sep = "")

  #---------------------------------------
  #---------------------------------------

  # Retain desired variables and order columns
  d <- d %>%
    select(all_of(vkeep)) %>%
    select(year, hid, any_of('pid'), weight, any_of(c('region', 'division')), state, starts_with('puma'), everything(), -starts_with('rep_'), starts_with('rep_'))  # Reorder columns with replicate weights at the end

  # Create dictionary
  cat("Creating dictionary\n")
  dictionary <- fusionData::createDictionary(data = d,
                                             survey = "ACS",
                                             vintage = year,
                                             respondent = ifelse(hus, "H", "P"))

  # Save dictionary to disk (.rds)
  cat("Saving dictionary to disk\n")
  fname <- paste0("ACS_", year, ifelse(hus, "_H", "_P"), "_dictionary.rds")
  saveRDS(object = dictionary,
          file = file.path("survey-processed/ACS", year, fname))

  # Save processed microdata to disk (.fst)
  cat("Saving processed microdata to disk\n")
  fst::threads_fst(nr_of_threads = 2L)
  fst::write_fst(x = d,
                 path = file.path("survey-processed/ACS", year, sub("dictionary.rds", "processed.fst", fname)),
                 compress = 100)

  return(dictionary)

}
