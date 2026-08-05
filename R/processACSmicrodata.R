#' Process ACS PUMS Microdata, Codebooks, and Custom Variables
#'
#' @name processACSmicrodata
#' @aliases processACSmicrodata
#'
#' @description
#' A unified suite of maintainer utilities in the \pkg{fusionData} pipeline designed to ingest,
#' parse, harmonize, and extend U.S. Census Bureau American Community Survey (ACS) Public Use
#' Microdata Sample (PUMS) raw data, official data dictionaries, and derived user-defined
#' custom variables.
#'
#' @details
#' \strong{Typical Execution Workflow:}
#' \enumerate{
#'   \item \strong{Primary Microdata Build (\code{processACSmicrodata})}:
#'         Once raw Census ZIP archives (\code{csv_hus.zip} and \code{csv_pus.zip}) are placed in
#'         \code{survey-raw/ACS/{year}/}, the standard operating procedure is to call
#'         \code{processACSmicrodata()} for both respondent units in a given vintage:
#'         \preformatted{
#'           processACSmicrodata(year = 2021, respondent = "H")
#'           processACSmicrodata(year = 2021, respondent = "P")
#'         }
#'         This orchestrates key standardization, financial inflation adjustments, geographic filtering,
#'         codebook label mapping, non-response imputation via \pkg{fusionModel}, and binary output generation.
#'   \item \strong{Internal Codebook Parsing (\code{processACScodebook})}:
#'         This function is called \emph{internally} by \code{processACSmicrodata()} to automatically
#'         locate and parse vintage-dependent data dictionary files (\code{.pdf}, \code{.txt}, or \code{.csv}).
#'         Users generally do not need to invoke \code{processACScodebook()} separately, except when
#'         debugging dictionary parsing discrepancies or testing raw Census codebook structures.
#'   \item \strong{Custom Variable Compilation (\code{processACScustom})}:
#'         Once \code{processACSmicrodata()} has executed successfully for both household (\code{H})
#'         and person (\code{P}) records, \code{processACScustom()} can be executed:
#'         \preformatted{
#'           processACScustom(year = 2021)
#'         }
#'         This dynamically scans, validates, and evaluates user-defined transformation scripts from
#'         \code{survey-processed/ACS/custom/*.R}, appending custom metrics to standalone sidecar files
#'         and updating the primary metadata dictionary.
#' }
#'
#' @section Directory Structure & File Conventions:
#' \preformatted{
#' survey-raw/ACS/{year}/
#' ├── Dict*.txt | Dict*.csv | Dict*.pdf  (Raw Census codebook dictionary)
#' ├── csv_hus.zip                         (Raw household PUMS microdata archive)
#' └── csv_pus.zip                         (Raw person PUMS microdata archive)
#'
#' survey-processed/ACS/
#' ├── custom/                             (User custom R scripts: e.g., poverty.R, race.R)
#' └── {year}/                             (Processed output destination)
#'     ├── ACS_{year}_{H|P}_dictionary.rds (R metadata dictionary data frame)
#'     ├── ACS_{year}_{H|P}_processed.fst  (Standardized binary PUMS microdata)
#'     └── ACS_{year}_{H|P}_custom.fst     (Custom derived variable sidecar)
#' }
#'
#' @param year Integer survey vintage (2005 onward).
#' @param respondent Character string specifying the respondent unit: \code{"H"} for Household-level
#'   records or \code{"P"} for Person-level records.
#' @param dictionary.file Character string specifying the path to a raw Census data dictionary file
#'   (\code{.pdf}, \code{.txt}, or \code{.csv}).
#' @param ... Additional arguments passed internally to processing routines or underlying file read/write operations.
#'
#' @return
#' Depending on the function invoked:
#' \itemize{
#'   \item \code{processACSmicrodata()}: Writes two production files to \code{survey-processed/ACS/{year}/}:
#'         \code{ACS_{year}_{H|P}_dictionary.rds} (metadata dictionary) and
#'         \code{ACS_{year}_{H|P}_processed.fst} (compressed microdata). Returns the processed data frame invisibly.
#'   \item \code{processACScodebook()}: Returns a structured \code{data.frame} or \code{data.table}
#'         containing standardized dictionary columns: \code{var} (variable name), \code{desc} (description),
#'         \code{value} (raw value), \code{label} (factor label), \code{adj} (inflation adjustment flag),
#'         and \code{custom_desc} (flag for manual descriptions).
#'   \item \code{processACScustom()}: Writes standalone sidecar datasets (\code{ACS_{year}_{H|P}_custom.fst})
#'         and updates the primary metadata file (\code{ACS_{year}_{H|P}_dictionary.rds}) with \code{custom = TRUE} tags.
#'         Invisibly returns the compiled custom data frame.
#' }
#'
#' @seealso \code{\link[fst]{read_fst}}, \code{\link[labelled]{set_variable_labels}}, \code{\link[fusionModel]{impute}}
#'
#' @export

# Clean and Standardize ACS PUMS Microdata Records
#
# PURPOSE & OVERVIEW FOR USERS:
# The processACSmicrodata() function is an internal, non-exported maintainer utility
# in the fusionData workflow. Its primary objective is to ingest raw American
# Community Survey (ACS) Public Use Microdata Sample (PUMS) CSV/ZIP archives
# for a specific survey vintage and respondent unit, transform raw coded
# responses into clean, fully labeled variables, resolve historical structural
# inconsistencies across vintages, impute missing values, and output standardized
# production files.
#
# SURVEY VINTAGES & RESPONDENT UNITS:
#   - 'year': Integer survey vintage (2005 onward).
#   - 'respondent': Respondent unit, specified as "H" (Household) or "P" (Person).
#
# KEY PROCESSING PHASES:
#   1. Directory Parsing & Codebook Standardizing: Locates raw Census ZIP/CSV archives
#      and runs processACScodebook() to generate a structured data dictionary.
#   2. SERIALNO Standardization: Converts raw Census alphanumeric SERIALNO strings
#      into clean 32-bit integer housing unit identifiers ('hid'), tagging Group
#      Quarters (GQ) vs. Housing Unit (HU) status via fixed prefixes.
#   3. Financial Inflation Adjustments: Scales monetary variables (income, housing costs)
#      using Census adjustment factors (ADJINC, ADJHSG).
#   4. Domain & Universe Filtering: Restricts records to valid U.S. States + D.C.
#      (dropping territories like Puerto Rico) and filters out vacant/non-sampled units.
#   5. Label Mapping & Factor Coercion: Replaces raw integer codes with human-readable
#      labels from the codebook, ordering ordinal factors appropriately.
#   6. Utility & Tax Harmonization: Standardizes utility cost flags and categorizes
#      legacy top-coded tax/value variables (e.g., TAXP -> TAXAMT).
#   7. Missing Data Imputation: Runs fusionModel::impute() to fill non-response
#      gaps across variables prior to downstream microdata matching.
#   8. Output Assembly: Constructs variable labels, renames geographic variables
#      to match census decade definitions (puma00, puma10, puma20), adds person-level
#      IDs ('pid'), and saves binary output files (.rds dictionary and .fst microdata).
#
# FILE LOCATIONS & EXPECTED INPUTS:
# Expects raw Census archives under 'survey-raw/ACS/{year}/' containing:
#   - Dict*.txt, Dict*.csv, Dict*.pdf (Census codebook dictionary)
#   - csv_hus.zip and csv_pus.zip (Raw microdata archives from Census Bureau)

# SOURCE DATA INFORMATION:
# Raw .csv data files downloaded from here:
#   https://www2.census.gov/programs-surveys/acs/data/pums/
#   Download "csv_hus.zip" and "csv_pus.zip" for nationwide microdata for given year (1-year)
#
# Data dictionary (.csv) files downloaded from here:
#   https://www.census.gov/programs-surveys/acs/microdata/documentation.html
#
# Example: PUMS_Data_Dictionary_2019.csv
# This is a .csv file but identified on website by an Excel icon
#
# For years prior to 2017, there is no .csv data dictionary available.
# Instead, must use the .txt file (e.g. PUMSDataDict15.txt)
#
# The PUMA variable has the vintage specified here, depending on the year:
#   https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.2020.html
#
# OUTPUTS GENERATED:
# Saves two files into 'survey-processed/ACS/{year}/':
#   - ACS_{year}_{H|P}_dictionary.rds: Data frame dictionary of processed variables.
#   - ACS_{year}_{H|P}_processed.fst: Highly compressed, standardized microdata file.

processACSmicrodata <- function(year, respondent = c("H", "P")) {

  if (basename(getwd()) != "fusionData") {
    cli::cli_abort("Function must be executed from within the {.file fusionData} directory.")
  }

  # Check input arguments
  stopifnot({
    year %% 1 == 0 & year >= 2005
    toupper(respondent) %in% c("H", "P")
  })

  # Processing household data?
  hus <- toupper(respondent) == "H"

  # Report which data is being processed
  cli::cli_h1("Processing {year} ACS-PUMS {ifelse(hus, 'Household', 'Person')}-level microdata")

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
  cli::cli_alert_info("Processing raw codebook data into useful format")
  codebook <- processACScodebook(dictionary.file)

  #---------------------------------------

  # Primary weights variable
  wvar <- ifelse(hus, "WGTP", "PWGTP")

  # Unzip raw .zip file
  cli::cli_alert_info("Un-zipping raw microdata files to temporary directory")
  tdir <- tempfile()
  unzip(data.zipfile, exdir = tdir, overwrite = TRUE)
  dfiles <- list.files(path = tdir, pattern = ".csv$", full.names = TRUE)

  # Read PUMS .csv data files
  cli::cli_alert_info("Reading raw microdata from disk")
  d <- dfiles %>%
    map_dfr(data.table::fread) %>%
    rename_with(toupper)   # Ensure upper-case names for consistency for 'codebook'; replicate weights are sometimes lower-case in the raw data

  # Delete temporary files
  unlink(tdir, recursive = TRUE)

  # Replace literal empty strings ("") with NA for character type columns
  # fread() does not convert empty strings to NA, as they are ambiguous
  for (i in 1:ncol(d)) {
    x <- d[[i]]
    if (is.character(x)) set(d, j = i, value = na_if(x, ""))
  }

  # Manual fix for 2023 onward to rename "STATE" to "ST" for consistency with earlier vintages
  if (year >= 2023) {
    d <- rename(d, ST = STATE)
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
  # Note: GQ units in housing records have weight (WGTP) set to zero
  gq <- filter(codebook, grepl("group quarters population", label))
  hu <- if (hus) d$WGTP > 0 else !d[[gq$var[1]]] %in% gq$value
  if (year == 2005) hu <- rep(TRUE, nrow(d))  # For 2005 only, there are no group quarter individuals (only housing units in the data)
  d[, SERIALNO := cleanACSID(SERIALNO, hu)]

  #-------------

  # Apply 'ADJHSG' and 'ADJINC' adjustment to appropriate variables
  # Note that this code makes no adjustments prior to 2008, as there was only a single ADJUST variable for dollar amounts
  if (year >= 2008) {
    v.adjhsg <- filter(codebook, var %in% names(d) & adj == "ADJHSG")$var
    v.adjinc <- filter(codebook, var %in% names(d) & adj == "ADJINC")$var
    d <- d %>%
      mutate_at(v.adjhsg, ~ round(.x * (ADJHSG / 1e6))) %>%
      mutate_at(v.adjinc, ~ round(.x * (ADJINC / 1e6)))
  }

  # If 'hus', remove group quarter observations AND vacant housing units
  # Note that person records retain individuals in group quarters, while household records have GQ because they have NA's for many variables
  # The person records do not include vacant housing units, by default
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
  cli::cli_alert_info("Making codebook consistent with the microdata")
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
    cli::cli_alert_warning("Identified potential issues in 'codebook':")
    print(issues)
    cli::cli_alert_info("These cases will have 'label' set to NA to force imputation.\nIf this is not correct, please STOP and correct the codebook processing code")
  }

  # Can insert additional, manual codebook fix-ups if necessary
  # By default, this simply sets missing 'label' entries to NA to force imputation later
  codebook <- codebook %>%
    mutate(label = ifelse(is.na(value) & label == "", NA, label)) # This sets any remaining blank labels to NA (to be imputed later)

  #---------------------------------------

  # Assign ACS labels from codebook to the data
  # Update the values in 'd' with labels from the codebook (i.e. replace the original integer values with text)
  cli::cli_alert_info("Assigning codebook labels to the microdata")

  # Specify which variables should be treated as ordered factors
  # In general, we want to coerce unordered factors to ordered factors whenever feasible (there is some judgement involved)
  # Note that which variables are ordered factors can potentially change over time
  of.hus <- c('ACR','AGS','BDS','MV','R18','R60','R65','RMS','VEH','WIF','YBL','YRBLT')
  of.pus <- c('CITWP','DECADE','DRAT','DRIVESP','ENG','GCM','JWAP','JWDP','JWRIP','MARHYP','MARHT','QTRBIR','SCHG','SCHL','SFN','WKL','YOEP')
  ordered.factors <- c(of.hus, of.pus)

  # Ordered factor variables present in the data
  ordfac <- intersect(ordered.factors, codebook$var)
  if (length(ordfac)) {
    cli::cli_alert_info("Treating the following variables as ordered factors: {.val {ordfac}}")
  }

  # Only retain variables remaining in the codebook
  dvars <- c(intersect(names(d), codebook$var))
  d <- d[, ..dvars]

  # Update variable values with associated labels from 'codebook'
  # Loop through each variable in 'd', assigning labels when applicable
  cli::cli_alert_info("Assigning labels to raw data")

  pb <- cli::cli_progress_bar("Applying codebook labels", total = length(dvars))
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
      if (sum(is.na(x)) != num.na) {
        cli::cli_alert_warning("{v}: introduced {sum(is.na(x))} NA values (subsequently imputed) due to values in raw data not being present in codebook. Should be reported to survey administrators.")
      }
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
    cli::cli_progress_update(id = pb)

  }
  cli::cli_progress_done(id = pb)

  #-----

  # Convert "ST" and "PUMA" to factors; they are coerced to integers by type.convert() above
  # This ensures that "ST" is treated as factor in imputation step
  d[, ST := factor(str_pad(ST, width = 2, pad = 0))]
  d[, PUMA := factor(str_pad(PUMA, width = 5, pad = 0))]

  #---------------------------------------

  if (hus) {

    # Adjust and modify housing-related variables
    cli::cli_alert_info("Adjusting housing variables")

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
    }

    # Convert categorical/factor property value variable prior to 2008 (VAL) to numeric value (topcoded at $1,000,000)
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

  if (hus) {

    # If prior to survey year 2018, add utility fuel cost flag variables to microdata
    # Prior to 2018, the utility expenditure variables (e.g. ELEP) included de facto categorical information about payment status as an integer entry
    # For example: "1" = "Included in rent or in condo fee"
    # Starting in 2018, Census Bureau assigns this information to separate "cost flag variables" that are easier to work with
    # The function utilityCostFlags() adds the cost flag variables for pre-2018 ACS vintages and revises the original expenditure variables as necessary
    # This creates consistent utility expenditure variables across years

    if (year < 2018) {

      cli::cli_alert_info("Adding utility cost flag variables")
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
    cli::cli_alert_info("Percentage of missing values:")
    print(round(na.count * 100, 2))

    cli::cli_alert_info("Imputing missing values")

    # Use impute() to impute missing values in 'd'
    ignore <- names(select(d, SERIALNO, PUMA, starts_with("WGTP"), starts_with("PWGTP")))
    d <- fusionModel::impute(d, weight = wvar, ignore = ignore)

  }

  #---------------------------------------

  # Assemble output
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost in the process
  cli::cli_alert_info("Assembling final output")
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
    cli::cli_alert_info("Adding person identifier (pid) variable")
    rvar <- tolower(filter(codebook, label == "Reference person")$var)  # Name of the household member relationship variable
    d <- addPID(data = d, hid = "hid", refvar = rvar)
  }

  #---------------------------------------

  # Add manual/custom variable definitions/labels for modified, undefined, or ambiguous variables
  cli::cli_alert_info("Assigning custom variable definitions")

  # Manual variable definitions; these need not be present in the data (silently ignored if not present)
  manual.defs <- list(
    year = "Survey year",
    hid = "Housing unit ID constructed from original PUMS SERIALNO",
    weight = ifelse(hus, "Housing unit central sampling weight", "Person central sampling weight"),
    state = "State FIPS code",
    puma00 = "Public use microdata area code based on 2000 census definition",
    puma10 = "Public use microdata area code based on 2010 census definition",
    puma20 = "Public use microdata area code based on 2020 census definition",
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
  drop <- setdiff(names(d), vkeep)  # Variables to be dropped due to absence of variable description
  if (length(drop)) {
    cli::cli_alert_warning("Removing the following undefined variable(s): {.val {drop}}")
  }

  #---------------------------------------

  # Retain desired variables and order columns
  d <- d %>%
    select(all_of(vkeep)) %>%
    select(year, hid, any_of('pid'), weight, any_of(c('region', 'division')), state, starts_with('puma'), everything(), -starts_with('rep_'), starts_with('rep_'))  # Reorder columns with replicate weights at the end

  # Create dictionary
  cli::cli_alert_info("Creating dictionary")
  dictionary <- fusionData::createDictionary(data = d,
                                             survey = "ACS",
                                             vintage = year,
                                             respondent = ifelse(hus, "H", "P"))

  # Save dictionary to disk (.rds)
  cli::cli_alert_info("Saving dictionary to disk")
  fname <- paste0("ACS_", year, ifelse(hus, "_H", "_P"), "_dictionary.rds")
  saveRDS(object = dictionary,
          file = file.path("survey-processed/ACS", year, fname))

  # Save processed microdata to disk (.fst)
  cli::cli_alert_info("Saving processed microdata to disk")
  fst::write_fst(x = d,
                 path = file.path("survey-processed/ACS", year, sub("dictionary.rds", "processed.fst", fname)),
                 compress = 100)

  cli::cli_alert_success("Processing finished for {year} ACS {ifelse(hus, 'Household', 'Person')}-level microdata.")

}
