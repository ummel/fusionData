#' Process ACS PUMS Microdata Records, Codebooks, and Custom Variables
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
#' @name processACSmicrodata
#' @aliases processACSmicrodata processACScodebook processACScustom
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

#-----------------------------
#-----------------------------
#-----------------------------

#' @describeIn processACSmicrodata Parse and normalize raw Census PUMS codebook dictionaries across PDF, TXT, and CSV formats
#'
#' @details
#' \bold{processACScodebook Mechanics:}
#' The Census Bureau distributes PUMS data dictionaries in different file formats depending on vintage:
#' \itemize{
#'   \item Pre-2012: Distributed primarily as raw unformatted \code{.pdf} files.
#'   \item 2012--2016: Distributed as fixed-width / structured \code{.txt} files.
#'   \item 2017 Onward: Distributed as structured \code{.csv} files.
#' }
#' \code{processACScodebook()} abstracts these format differences into a unified output table, identifying
#' value ranges, applying human-readable level descriptions, and flagging variables subject to inflation
#' adjustment factors (\code{ADJINC} or \code{ADJHSG}).
#'
#' @export

# Process and Standardize ACS PUMS Codebooks
#
# PURPOSE & OVERVIEW FOR USERS:
# As part of the fusionData microdata build pipeline, raw American Community Survey
# (ACS) Public Use Microdata Sample (PUMS) codebooks and data dictionaries are
# processed into a standardized, machine-readable format.
#
# ACS codebooks distributed by the U.S. Census Bureau vary across survey vintages:
#   - Pre-2012 vintages are primarily distributed as .pdf files.
#   - 2012-2016 vintages are distributed as structured .txt files.
#   - 2017 onward vintages are distributed as structured .csv files.
#
# The processACScodebook() function automates the parsing, cleaning, and
# normalization of these disparate dictionary files into a uniform data frame
# structure (codebook) containing:
#   - 'var': The standardized ACS variable identifier (e.g., "HINCP", "TEN", "ST").
#   - 'desc': A clear, standardized description of what the variable measures.
#   - 'value': The raw encoded code/value (e.g., "0", "1", "100..500").
#   - 'label': Human-readable factor level label corresponding to the value.
#   - 'adj': Indicates if inflation adjustment factors (ADJHSG/ADJINC) apply.
#   - 'custom_desc': Flag indicating if a manually refined description was applied.
#
# USAGE CONTEXT:
# This is an internal maintainer function used when processing new ACS survey
# vintages for the fusionData and fusionACS ecosystem.

# Example inputs
# dictionary.file <- "survey-raw/ACS/2005/PUMSDataDict05.pdf"
# dictionary.file <- "survey-raw/ACS/2015/PUMSDataDict15.txt"
# dictionary.file <- "survey-raw/ACS/2019/PUMS_Data_Dictionary_2019.csv"

processACScodebook <- function(dictionary.file) {

  if (basename(getwd()) != "fusionData") {
    cli::cli_abort("Function must be executed from within the {.file fusionData} directory.")
  }

  # Internal helper function to capitalize the first letter of a string
  capFirst <- function(x) {
    str_sub(x, 1, 1) <- toupper(str_sub(x, 1, 1))
    return(x)
  }

  # Internal helper function to extract text enclosed within parenthetical expressions
  parText <- function(x) {
    x <- sub(".*\\(", "", x)  # Remove everything up to and including the first "("
    i <- which(str_sub(x, -1) == ")")
    x[i] <- str_sub(x[i], 1, -2)  # Remove trailing ")", if present
    return(x)
  }

  # ----------

  # Validate file format
  suffix <- str_sub(dictionary.file, start = -4)
  stopifnot(suffix %in% c(".csv", ".txt", ".pdf"))

  cli::cli_alert_info("Processing ACS codebook from {.file {dictionary.file}}...")

  # Ingest dictionary file according to its native format
  ddata <- if (suffix == ".csv") {
    # 2017+ CSV dictionary files have a standard tabular layout
    read.csv(dictionary.file, header = FALSE, na.strings = "") %>%
      setNames(c('record', 'var', 'type', 'length', 'value', 'value2', 'label')) %>%
      select(var, value, value2, label)
  } else {
    if (suffix == ".txt") {
      # 2013-2016 plain text dictionary files parsed via custom text parser
      convertTXTdictionary(dictionary.file)
    } else {
      # Pre-2013 PDF dictionary files are converted to raw text streams first,
      # cleaned of page headers/footers, and then processed by convertTXTdictionary()
      x <- pdftools::pdf_text(dictionary.file)
      x <- gsub("\n+[ ]*[0-9]{1,3}\n$", "\n", x)  # Strip PDF page footer numbers
      x <- trimws(strsplit(paste(x, collapse = ""), split = "\n")[[1]])
      convertTXTdictionary(x)
    }
  }

  # ----------

  # Process, filter, and normalize raw dictionary lines into standardized codebook entries
  codebook <- ddata %>%

    distinct() %>%   # Deduplicate rows shared between household and person-level headers

    split(f = .$var) %>%

    # Identify range values, blank/missing placeholders, and literal values
    map(~ mutate(.x,
                 rng = (value != value2 & !is.na(value2)),
                 miss = grepl("^(b)\\1*$", value),  # Matches Census whitespace representations "b", "bb", etc.
                 value = ifelse(miss, NA, value),  # Convert whitespace codes to true NA
                 asis = !rng & !miss)) %>%

    # Reconstruct variable-level metadata tables with unified value and label columns
    map_dfr(~ tibble(var = .x$var[1],
                     desc = .x$value[1],
                     value = if (all(.x$asis)) {.x$value[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$value[.x$miss], if (any(.x$rng)) {NULL} else {.x$value[!.x$miss][-1]})}},
                     label = if (all(.x$asis)) {.x$label[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$label[.x$miss], if (any(.x$rng)) {NULL} else {.x$label[!.x$miss][-1]})}})) %>%

    mutate_if(is.character, str_squish) %>%  # Clean up whitespace around parsed strings

    # Filter out redundant, technical allocation, or superseded Census variables
    filter(
      !(str_sub(var, 1, 3) == "RAC" & label %in% c("Yes", "No")),  # Drop binary race recodes (covered comprehensively by RAC1P)
      !grepl("allocation flag", tolower(desc), fixed = TRUE),  # Drop post-2008 item allocation flag variables
      !grepl("allocation$", tolower(desc)),  # Drop pre-2008 item allocation flag variables
      !grepl("eligibility coverage edit", desc, fixed = TRUE),  # Drop internal health insurance edit flags
      !grepl("See 'Employment Status Recode' (ESR)", desc, fixed = TRUE),  # Drop redundant intermediate employment questions
      !grepl("^MLP.", var)  # Drop legacy military period flags (subsumed by 'VPS')
    ) %>%

    # Secondary filter pass for allocation flags
    # Catches edge cases where text formatting glitches in legacy .txt dictionaries shifted description text into the value column
    group_by(var) %>%
    mutate(alloc_flag1 = any(grepl("allocation flag", tolower(value), fixed = TRUE)), # Post-2008
           alloc_flag2 = any(grepl("allocation$", tolower(value)))) %>%  # 2008 and earlier
    ungroup() %>%
    filter(!alloc_flag1, !alloc_flag2) %>%

    # Secondary filter pass for race recodes caught by value column anomalies
    group_by(var) %>%
    mutate(recode_flag = str_sub(var[1], 1, 3) == "RAC" & any(grepl("combination", paste(desc, value), fixed = TRUE))) %>%
    ungroup() %>%
    filter(!recode_flag) %>%

    # Coerce character representation of numeric codes to standard integer storage
    # and tag variables that require financial adjustment factors (ADJHSG/ADJINC)
    mutate(
      temp = suppressWarnings(as.integer(value)),
      value = ifelse(is.na(temp), value, as.character(temp)),  # Ensures integer representation matches data imported via data.table::fread()
      temp = NULL,
      adj = ifelse(grepl("use adjhsg", tolower(desc)), "ADJHSG", NA),
      adj = ifelse(grepl("use adjinc", tolower(desc)), "ADJINC", adj)
    ) %>%

    # Standardize variable descriptions for clarity and readability
    mutate(
      desc = gsub("\\s*\\([^\\)]+\\)", "", desc),  # Strip parenthetical annotations from descriptions
      desc = gsub("recode", "", desc),  # Drop redundant word 'recode'
      desc = gsub("HH", "household", desc),  # Expand abbreviation 'HH' to 'household'
      desc = ifelse(var %in% c("RMS", "RMSP"), "Number of rooms, excluding bathrooms", desc),  # Explicit clarification for room counts
      desc = gsub("write-in", "", desc, fixed = TRUE),  # Remove unnecessary "write-in" flags
      desc = gsub("english", "English", desc),  # Capitalize proper nouns
      desc = ifelse(desc == "VA", "VA health care", desc),  # Expand abbreviated health system labels
      desc = ifelse(desc == "Indian health service", "Indian Health Service", desc),
      desc = gsub(" puma ", " PUMA ", desc),
      desc = gsub(" soc codes", " SOC codes", desc),
      desc = gsub(" naics codes", " NAICS codes", desc),
      desc = gsub(" ind codes", " IND codes", desc),
      desc = capFirst(desc)
    ) %>%

    # Standardize value labels (e.g., FIPS codes, factor levels, parenthetical parsing)
    mutate(
      label = ifelse(var %in% c("ST", "STATE"), value, label), # Replace full state text names with FIPS codes
      label = gsub("/ ", "/", label, fixed = TRUE),  # Fix irregular slash formatting
      label = gsub(" / ", "/", label, fixed = TRUE),
      label = gsub("//", "/", label, fixed = TRUE),
      label = ifelse(str_sub(label, 1, 3) == "N/A", parText(label), label),   # Convert "N/A (description)" labels to extracted parenthetical text
      label = gsub(" FT", " full-time", label, fixed = TRUE),
      label = gsub("NILF ", "Not-in-labor-force ", label, fixed = TRUE),
      label = gsub("<", "less than", label, fixed = TRUE),
      noedit = grepl("/[0-9]", label) | is.na(label),
      label = ifelse(noedit, label, map_chr(strsplit(label, split = "/"), ~ paste(capFirst(.x[!tolower(.x) %in% c("gq", "vacant")]), collapse = " / "))),
      noedit = NULL,
      label = ifelse(grepl("suppress", tolower(label)), NA, label),  # Replace administrative suppression notes with NA
      label = sub("^\\.", "", label),  # Strip leading period artifacts
      label = capFirst(label)
    ) %>%

    # Manually drop variables containing obsolete or non-informative metrics
    filter(!var %in% c("RT", "DECADE", "SRNT", "SVAL", "OCPIP", "GRPIP", "DRIVESP", "DRATX", "SPORDER", "WAOB", "MRGX", "SMX")) %>%

    # Apply manual fixes for known variable level edge cases across survey years
    mutate(
      label = ifelse(is.na(value) & var %in% c("BROADBND", "DIALUP", "HISPEED", "DSL", "FIBEROP", "MODEM", "OTHSVCEX", "SATELLITE"), "No paid access to the internet", label),
      label = ifelse(is.na(value) & var == "CPLT", "No couple present", label),  # Fix error in raw Census dictionary
      label = ifelse(is.na(value) & var == "RNTM", "No", label),  # Fix non-renter meal inclusion indicator
      label = ifelse(is.na(value) & var == "LANP", "GQ/Vacant", label),  # Restore missing label lost due to original parenthetical formatting
      value = ifelse(value == 0 & var == "DRAT", "1", value),  # Typo fix specific to 2008 PUMS dictionary
      label = ifelse(is.na(value) & var == "DRAT", "No service-connected disability/never served in military", label),
      value = ifelse(grepl("age less than 15 years", value) & var == "MARHYP", NA, value),
      label = ifelse(is.na(value) & var == "MARHYP", "Age less than 15 years; never married", label)
    ) %>%

    add_count(var) %>%
    mutate(label = ifelse(n == 1 & is.na(value) & !is.na(label), 0, label)) %>%
    mutate_if(is.character, str_squish) %>%
    distinct() %>%   # Final deduplication pass
    select(var, desc, value, label, adj)

  # -----

  # Standardize variable name for State FIPS codes across all vintages
  # Renames post-2023 "STATE" back to "ST" for consistency with earlier PUMS datasets
  if ("STATE" %in% codebook$var) {
    codebook <- codebook %>%
      mutate(var = ifelse(var == "STATE", "ST", var))
  }

  # -----

  # Apply curated, authoritative descriptions to key microdata variables
  # Overrides raw Census dictionary descriptions with clearer, project-standard terms
  vardefs <- list(
    DIVISION = "Census division",
    REGION = "Census region",
    NP = "Number of people in household",
    FS = "Food stamp recipient in household",
    HHT2 = "Household/family type, including cohabiting",
    MRGP = "Payment on first mortgage, monthly",
    SMP = "Payment on all second and junior mortgages and home equity loans, monthly",
    TEL = "Telephone service",
    FINCP = "Family income in the past 12 months",
    HINCP = "Household income in the past 12 months",
    CONP = "Condo fee, monthly",
    ELEP = "Electricity cost last month",
    FULP = "Fuel cost (oil, kerosene, wood, etc.) in the past 12 months",
    GASP = "Gas cost (pipeline, bottled, or tank) last month",
    GRNTP = "Gross rent including utilites, monthly",  # Source: https://www.census.gov/quickfacts/fact/note/US/HSG860221
    INSP = "Home insurance, annual",
    MHP = "Mobile home cost (site rent, fees, etc.), annual",
    RNTP = "Contract rent, monthly",
    TAXP = "Real estate taxes, annual",
    TAXAMT = "Real estate taxes, annual",
    VAL = "Property value reported by owner, zero for renter-occupied units",
    VALP = "Property value reported by owner, zero for renter-occupied units",
    WATP = "Water and sewer cost in the past 12 months",
    HFL = "Primary heating fuel",
    TEN = "Housing tenure",
    DSL = "DSL service",
    RELP = "Relationship to reference person",
    RELSHIPP = "Relationship to reference person",
    RAC1P = "Race, detail level 1",
    RAC2P = "Race, detail level 2",
    RAC3P = "Race, detail level 3"
  )

  codebook$custom_desc <- FALSE
  for (v in names(vardefs)) {
    i <- codebook$var == v
    codebook$desc[i] <- vardefs[[v]]
    codebook$custom_desc[i] <- TRUE
  }

  # -----

  cli::cli_alert_success("Processed {.val {length(unique(codebook$var))}} variables from codebook.")

  return(codebook)

}

# ------------

# Helper function to convert raw .txt dictionary files (pre-2017) into a
# standardized tabular format mimicking modern .csv dictionary releases.

convertTXTdictionary <- function(input) {

  if (file.exists(input[[1]])) {
    d <- readLines(input, warn = FALSE)
  } else {
    d <- input
  }

  # Locate start line based on "RT" (Record Type) being the first listed variable
  start <- which(substring(d, 1, 2) == "RT")[1]

  # Locate end line (last replicate weight variable 'PWGTP80' plus buffer lines)
  finish <- which(substring(d, 1, 7) == "PWGTP80") + 2L

  d <- d[start:finish]

  # -----

  # Standardize character encoding artifacts and non-standard punctuation
  quote.chars <- c("\x92", "\x93", "\x94", "\\92", "\\93", "\\94")
  for (x in quote.chars) d <- gsub(x, "'", d, fixed = TRUE, useBytes = TRUE)

  # Clean invalid byte sequences and non-breaking spaces
  d <- gsub("\xa0", "", d, fixed = TRUE, useBytes = TRUE)

  # Standardize en-dashes and specialized hyphens
  d <- gsub("\x96", "-", d, fixed = TRUE, useBytes = TRUE)
  d <- gsub("\\96", "-", d, fixed = TRUE, useBytes = TRUE)
  d <- gsub("\xe2\x80'", "-", d, fixed = TRUE, useBytes = TRUE)

  # Convert erroneous hyphenated or lettered range representations to standard double-dot notation ('..')
  d <- gsub(" - 9", "..9", d, fixed = TRUE)
  d <- gsub(" B 9", "..9", d, fixed = TRUE)

  # Remove formatting asterisks
  d <- gsub("\\*+", "", d)

  # Separate run-together military occupation codes to allow clean string splitting
  mil.chars <- paste0("928110P", 1:7)
  for (x in mil.chars) d <- sub(x, paste0(x, " "), d, fixed = TRUE)

  # Clean tab character artifacts
  d <- gsub("$\t", "$", d, fixed = TRUE)

  # Convert tabs into pipe ('|') delimiters for structured token splitting
  d <- gsub("\t", "|", d, fixed = TRUE)

  # Strip trailing pipes at end of lines
  d <- sub("\\|$", "", d)

  # Insert padding space before ".N/A" to ensure clean tokenization (e.g., NAICSP variable)
  d <- sub(".N/A", " .N/A", d, fixed = TRUE)

  # -----

  # Detect and reattach orphaned line wraps (lines beginning with '.') back to their preceding header line
  temp <- trimws(gsub("|", "", d, fixed = TRUE))
  k <- which(substring(temp, 1, 1) == ".")
  d[k] <- substring(temp[k], first = 2)
  for (i in rev(k)) d[i - 1] <- paste(d[i - 1], d[i])
  if (length(k)) d <- d[-k]

  # -----

  # Convert value descriptor delimiters (" .") into pipes for splitting
  d <- gsub(" .", "|", d, fixed = TRUE)

  # Clean extra internal whitespace
  d <- stringr::str_squish(d)

  # Remove structural header lines and blank spacer lines from raw text
  d <- d[!grepl("intentionally blank", d, fixed = TRUE)]
  d <- d[!grepl("PERSON RECORD", d, fixed = TRUE)]

  # -----

  # Split lines into character vectors based on pipe delimiters
  d <- lapply(d, strsplit, split = "\\|+")
  d <- map(d, ~ .x[[1]][str_squish(.x[[1]]) != ""])

  # -----

  # Identify indices corresponding to the start of new variable entries
  # Matches variable name + position width (e.g., "ANC1P 3") or isolated line numbers
  ind1 <- grep("^[A-Z][A-Z0-9]*\\s[0-9]$", d)
  ind2 <- which(map_lgl(d, ~ grepl("^[A-Z][A-Z0-9]*", .x[1]) & length(.x) == 2 & !is.na(suppressWarnings(as.numeric(.x[2])))))
  ind3 <- grep("^[0-9]$", d) - 1L  # Handles PDF conversion errors where position integer was pushed to a new line
  ind <- sort(unique(c(ind1, ind2, ind3)))

  # -----

  # Inner helper function to parse values, ranges, and labels for a single variable block
  parseEntry <- function(i) {

    # Extract lines belonging to variable entry 'i'
    if (i == length(ind)) {
      x <- d[ind[i]:length(d)]
    } else {
      x <- d[ind[i]:(ind[i + 1] - 1)]
    }

    x <- purrr::compact(x)

    # Extract and append parenthetical "Note:" text if present in the variable block
    note <- map_lgl(x, ~ tolower(substring(.x[1], 1, 5)) == "note:")

    if (any(note)) {
      note.text <- paste(unlist(x[which(note)[1]:length(x)]), collapse = " ")
      note.text <- paste0("(", str_squish(substring(note.text, first = 6)), ")")
      drop <- c(1, 2, which(note)[1]:length(x))
    } else {
      note.text <- NULL
      drop <- c(1, 2)
    }

    y <- x[-drop]

    # Parse individual value ranges and labels
    parseValuesLabel <- function(x) {
      z <- unlist(strsplit(x[1], "..", fixed = TRUE))
      out <- str_squish(c(z[1], ifelse(length(z) == 1, z[1], z[2]), sub(" .", "", x[2], fixed = TRUE)))
      return(out)
    }

    m <- lapply(y, parseValuesLabel)
    m <- do.call(rbind, m)

    m <- rbind(c(paste(x[[2]], note.text), NA, NA), m)
    m <- cbind(x[[1]][1], m)
    colnames(m) <- c("var", "value", "value2", "label")
    as.data.frame(m)

  }

  # Parse all identified variable entries and stack into a unified data frame
  result <- map_dfr(seq_along(ind), parseEntry)

  # Clean variable identifier to retain only the leading uppercase string
  result$var <- word(result$var, start = 1, end = 1)

  return(result)

}

#-----------------------------
#-----------------------------
#-----------------------------

#' @describeIn processACSmicrodata Compile user-defined custom derived variables from external script sidecars
#'
#' @details
#' \bold{processACScustom Mechanics & Script Requirements:}
#' Standard PUMS microdata often lacks domain-specific or recoded metrics required for downstream
#' modeling (e.g., custom poverty ratios, recoded race/ethnicity categories, housing cost burdens).
#' \code{processACScustom()} automates the execution and validation of these derived variables:
#' \enumerate{
#'   \item \strong{Script Scanning}: Scans \code{survey-processed/ACS/custom/*.R} for individual R scripts.
#'   \item \strong{Function Contract}: Each custom script must define a function named identically to
#'         its filename (e.g., \code{poverty.R} defines \code{poverty()}). The function must accept
#'         \code{year} as its sole argument and return a data frame containing primary keys (\code{hid},
#'         and optionally \code{pid}).
#'   \item \strong{Validation Checks}: Guarantees that zero missing (\code{NA}) values exist in output
#'         columns, verifies that explicit variable labels are assigned via \code{\link[labelled]{var_label}},
#'         filters records to occupied housing units, and prevents column name collisions across scripts.
#' }
#'
#' @export

# Process and Compile Custom ACS Microdata Variables
#
# PURPOSE & OVERVIEW FOR USERS:
# The processACScustom() function is an internal, non-exported maintainer utility
# in the fusionData pipeline. Standard ACS microdata processed via processACSmicrodata()
# contains variables natively provided by the U.S. Census Bureau. However, fusionACS
# workflows frequently require user-defined, derived, or harmonized "custom" variables
# (e.g., specific housing classifications, income adjustments) that are not present in raw Census files.
#
# HOW IT WORKS:
# This function dynamically scans for individual R script files stored within
# 'survey-processed/ACS/custom/'. Each custom R script must define a function named
# identically to its filename, accepting 'year' as its single argument, and returning
# a data frame containing custom variables along with primary keys ('year', 'hid', and
# optionally 'pid').
#
# KEY PROCESSING PHASES:
#   1. Script Ingestion & Validation: Iterates through each .R file in the custom directory,
#      sources it, verifies argument constraints, and executes the script for the specified vintage.
#   2. Level Detection & Universe Harmonization: Checks output keys ('hid' vs. 'pid') against
#      previously processed ACS microdata files to automatically distinguish person-level
#      from household-level data and filters household observations to occupied units.
#   3. Label Enforcement & Cleaning: Validates that all derived custom variables possess explicit
#      variable labels via labelled::var_label(), formats numeric tolerances, and cleans factors.
#   4. Aggregation & Conflict Resolution: Combines output across multiple custom scripts
#      separately for person- and household-level records, checking for naming collisions.
#   5. Dictionary Updating & Storage: Appends custom variable definitions into the primary
#      data dictionary (.rds) with custom=TRUE tags and saves custom microdata sidecars (.fst).
#
# EXPECTED INPUTS & DIRECTORY STRUCTURE:
#   - 'year': Integer survey vintage (e.g., 2024).
#   - Custom R scripts located in 'survey-processed/ACS/custom/*.R'. Each function must:
#       - Accept 'year' as its sole argument.
#       - Return a data frame containing 'hid' (household ID) and optionally 'pid' (person ID).
#       - Ensure zero missing (NA) values in the output.
#       - Include variable labels assigned via labelled::var_label().
#   - Previously generated processed microdata files in 'survey-processed/ACS/{year}/'
#     (e.g., ACS_{year}_P_processed.fst and ACS_{year}_H_processed.fst).
#
# OUTPUTS GENERATED:
# Updates and creates sidecar files in 'survey-processed/ACS/{year}/':
#   - Updates 'ACS_{year}_{H|P}_dictionary.rds': Appends custom variable metadata.
#   - Creates 'ACS_{year}_{H|P}_custom.fst': Standalone microdata store holding custom variables.

processACScustom <- function(year) {

  if (basename(getwd()) != "fusionData") {
    cli::cli_abort("Function must be executed from within the {.file fusionData} directory.")
  }

  cli::cli_h1("Processing custom ACS variables for vintage {year}")

  # Identify all custom calculation R scripts available in the custom directory
  flist <- list.files("survey-processed/ACS/custom", pattern = "\\.R$", full.names = TRUE)

  if (length(flist) == 0) {
    cli::cli_alert_warning("No custom function scripts found in 'survey-processed/ACS/custom'")
    return(invisible(NULL))
  }

  # Execute and evaluate each custom processing script individually
  out <- lapply(flist, function(fun) {

    # Extract function name from path, load code into environment, and validate formals
    source(fun)
    fname <- sub("\\.R$", "", basename(fun))
    f <- get(fname)
    a <- names(formals(f))
    if (length(a) != 1 | a[1] != "year") {
      stop("The custom function ", fname, "() must have 'year' as its lone argument", sep = "")
    }

    # Execute custom computation routine wrapped in error handling
    cli::cli_alert_info("Executing custom function from {.file {basename(fun)}} for year {year}")
    out <- try(expr = f(year = year), silent = TRUE)

    # Process and validate successfully evaluated data frame returns
    if (inherits(out, "data.frame")) {

      # Enforce mandatory metadata and key structures
      out$year <- as.integer(year)
      if (!'hid' %in% names(out)) stop("Custom function in ", basename(fun), " must return 'hid' variable in result")
      if (anyNA(out)) stop("NA values are not allowed in the custom function output from ", basename(fun))

      # Standardize row ordering across primary keys to support deterministic comparisons
      out <- arrange(out, across(any_of(c('hid', 'pid'))))

      # Compare record identifiers against baseline processed microdata to determine data level
      pfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_P_processed.fst"), recursive = TRUE, full.names = TRUE)
      if (length(pfile) == 0) stop("Could not locate processed person microdata file for year ", year)

      p.hid <- fst::read_fst(pfile, columns = "hid")[[1]]

      if ('pid' %in% names(out) & identical(p.hid, out$hid)) {
        cli::cli_alert_info("  -- Classifying output as person-level records")
      } else {
        h.hid <- fst::read_fst(sub("_P_", "_H_", pfile), columns = "hid")[[1]]
        if (!'pid' %in% names(out) & all(h.hid %in% out$hid)) {
          cli::cli_alert_info("  -- Classifying output as household-level records")
          # Filter household records to match active occupied unit universe
          out <- filter(out, hid %in% h.hid)
        } else {
          stop("Unable to align output from ", basename(fun), " with standard person or household record keys")
        }
      }

      # Confirm that all newly introduced custom variables possess required descriptive labels
      cvars <- setdiff(names(out), c('year', 'hid', 'pid'))
      labs <- labelled::var_label(out)[cvars]
      miss <- setdiff(cvars, names(labs))
      if (length(miss)) stop("The following custom variables in ", basename(fun), " are missing labels: ", paste(miss, collapse = ", "))

      # Clean types, format floating points within tolerance, and re-apply variable metadata
      out <- out %>%
        mutate_if(is.factor, safeCharacters) %>%
        mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
        labelled::set_variable_labels(.labels = labs, .strict = FALSE) %>%
        arrange(across(any_of(c('hid', 'pid'))))

      cli::cli_alert_success("  -- Custom script {.file {basename(fun)}} successfully executed")
      return(out)

    } else {
      # Log execution failures gracefully and return NULL to be filtered out
      cli::cli_alert_danger("  -- Custom function {.file {basename(fun)}} failed execution")
      return(NULL)
    }

  })

  #---

  cli::cli_alert_info("Merging outputs across custom calculation scripts")

  # Remove failed execution results
  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) {
    cli::cli_alert_warning("No custom functions executed successfully. Exiting without saving.")
    return(invisible(NULL))
  }

  # Segregate and recursively combine custom calculations by respondent level
  pi <- map_lgl(out, ~ "pid" %in% names(.x))

  # Merge person-level custom variables on year, hid, and pid
  p <- if (any(pi)) reduce(out[pi], left_join, by = c('year', 'hid', 'pid'), relationship = 'one-to-one', suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(p), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during person-level merge: ", paste(dupe, collapse = ", "))

  # Merge household-level custom variables on year and hid
  h <- if (any(!pi)) reduce(out[!pi], left_join, by = c('year', 'hid'), relationship = 'one-to-one', suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(h), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during household-level merge: ", paste(dupe, collapse = ", "))

  # Write combined results to disk and update global data dictionaries
  for (i in c('h', 'p')) {
    d <- get(i)
    if (!is.null(d)) {

      level_str <- ifelse(i == "h", "Household", "Person")

      # Build dictionary data structure reflecting custom additions
      cli::cli_alert_info("Generating dictionary for custom {level_str}-level variables")
      dict <- fusionData::createDictionary(data = d, survey = "ACS", vintage = year, respondent = toupper(i), custom = TRUE)

      # Read base dictionary, strip previous custom entries if re-running, append new entries, and save
      cli::cli_alert_info("Updating dictionary file on disk")
      fname <- file.path("survey-processed/ACS", year, paste("ACS", year, toupper(i), "dictionary.rds", sep = "_"))

      # Retrieve non-custom variables directly from primary processed microdata file
      vars <- names(fst::fst(sub("dictionary.rds", "processed.fst", fname)))

      readRDS(fname) %>%
        filter(variable %in% vars) %>%
        mutate(custom = FALSE) %>%
        bind_rows(dict) %>%
        arrange(variable) %>%
        saveRDS(file = fname)

      # Export custom variables microdata archive to sidecar .fst file
      cli::cli_alert_info("Saving custom {level_str}-level microdata to disk")
      fst::write_fst(x = d, path = sub("dictionary.rds", "custom.fst", fname), compress = 100)

    }
  }

  cli::cli_alert_success("Custom ACS variable processing complete for vintage {year}")

}
