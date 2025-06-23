# Called manually to create pre-processed versions of raw data files for each survey year
# Only the hard-coded variables are retained, so adding a variable to the microdata requires specifying it in the appropriate place below and re-running processRawCEI()

library(fusionData)
library(tidyverse)

source("R/utils.R")

# Example usage:
# processRawCEI(2015)
# processRawCEI(2016)
# processRawCEI(2017)
# processRawCEI(2018)
# processRawCEI(2019)
#----------------------

processRawCEI <- function(survey_year) {

  # Dependency
  load("survey-processed/CEX/var_info.rda")

  # File-specific unique ID variables
  file.ids <- list(

    fmli = c("NEWID", "FINLWT21"),
    memi = c("NEWID", "MEMBNO"),
    #fmld = c("NEWID", "FINLWT21"),
    #memd = c("NEWID", "MEMBNO"),
    apa = c("NEWID", "QYEAR"),
    apb = c("NEWID", "QYEAR"),
    ovb = c("NEWID", "QYEAR", "VEHICIB"),
    lsd = c("NEWID", "QYEAR", "LSDNUM"),
    rnt = c("NEWID", "QYEAR")

    # Ignoring detailed tax estimation variables
    #ntaxi = c("NEWID", "TAX_UNIT")

    # NOTE HHM and HIM files appear to contain health insurance enrollment status, but they begin only in 2017 (so ignored for now)

  )

  # File-specific variables to select (other than ID's)
  file.vars <- list(

    fmli = c("HHID", "AGE_REF", "BATHRMQ", "BEDROOMQ", "BLS_URBN", "BUILDING", "BUILT",
             "CUTENURE", "DIVISION", "FAM_SIZE", "FINCBTXM", "FINATXEM",
             "FRRETIRM", "FSALARYM", "FSMPFRXM", "FSSIXM",  "HH_CU_Q", "HISP_REF",
             "HLFBATHQ", "INTRDVXM", "JFS_AMTM", "NETRENTM",
             "OTHREGXM", "OTHRINCM", "POPSIZE", "QINTRVMO", "QINTRVYR", "PSU",
             "REF_RACE", "REGION", "RENTEQVX", "RETSURVM", "ROOMSQ", "ROYESTXM",
             "SMSASTAT", "ST_HOUS", "STATE", "UNISTRQ", "VEHQ", "VEHQL", "WELFAREM",
             "SWIMPOOL", "RENDWECQ", "RENDWEPQ", "FMLPYYRX", "STUDFINX", "STUDNTX", "STUDNTB",
             "STOCKX", "STOCKB", "LIQUIDX", "LIQUIDB", "IRAX", "IRAB",
             "WINDOWAC", "CNTRALAC", "AS_COMP1", "AS_COMP2", "AS_COMP3", "AS_COMP4", "AS_COMP5",
             "NO_EARNR", "PERSLT18", "PERSOT64", "SEX_REF", "EDUC_REF", "INCNONW1", "INCOMEY1", "INCWEEK1",
             "MARITAL1", "OCCUCOD1", "OTHASTX", "OTHASTB", "WHOLIFX", "WHOLIFB",
             "CREDFINX", "CREDITB", "CREDITX", "OTHFINX", "OTHLONB", "OTHLONX",
             paste0("WTREP", str_pad(1:44, width = 2, pad = 0))), # Replicate weight variables

    # currently ignored:
    #"CREDFINX", "CREDITX", "FDAWAYCQ", "FDAWAYPQ", "FDHOMECQ", "FDHOMEPQ", "ALCBEVCQ", "ALCBEVPQ", "TTOTALC", "TTOTALP"
    #"FS_MTHI", "IRAYRB", "LUMPSUMX", "MEALSPAY", "OTHASTX", "OTHFINX", "OTHLOAN", "OTHLONX",
    #"fftaxowe", "fstaxowe", "WHOLIFX", "FJSSDEDM", "FMLPYYRX", "TOTXEST", "MISCTAXX", "DEFBENRP", "MLPAYWKX", "MLPYQWKS"

    memi = c("AGE", "ARM_FORC", "CU_CODE", "EARNER", "EDUCA",
             "HORIGIN", "IN_COLL", "INC_HRSQ", "INCNONWK", "INCOMEY", "INCWEEKQ",
             "MARITAL", "MEMBRACE", "OCCUCODE", "SALARYXM", "SEMPFRMM", "SEX",
             "SOCRRXM", "SSIXM", "TAX_UNIT", "TU_CODE"),
    # currently ignored:
    # "EMPLCONT", "GOVRETX", "INDRETX", "JSSDEDXM", "PRIVPENX", "SLFEMPSM", "SSNORM"

    # fmld = c("HHID", "BLS_URBN", "CUTENURE", "DESCRIP", "FAM_SIZE", "FAM_TYPE", "JFS_AMTM", "FINCBEFM", "FSMPFRXM",
    #          "FOODAWAY", "FOODHOME", "VEHQ", "FWAGEXM", "INTRDVXM", "FSS_RRXM", "FSUPPXM", "NETRENTM", "ROYESTXM",
    #          "RETSURVM", "OTHREGXM", "HH_CU_Q", "STATE"),
    #
    # memd = c("AGE", "ARM_FORC", "EDUCA", "HORIGIN", "IN_COLL", "MARITAL", "JSSDEDXM", "MEMBRACE",
    #          "SEMPFRMM", "SEX", "SOCRRXM", "WAGEXM", "OCCUEARN", "HRSPERWK", "SUPPXM", "EMPLTYPE"),

    apa = c("GFTC_MAJ", "MAJAPPLY", "MAJPURX"),

    apb =  c("GFTCMIN", "MINAPPLY", "MINPURX"),

    ovb = c("VEHICYB", "FUELTYPE", "MAKE", "VEHICYR", "NETPURX", "TRADEX", "PRINCIPX", "VEHNEWU", "VPURINDV", "VEHPURYR", "VFINANCE", "VEHQPMT", "VINTRATE", "VPURINDV"),

    lsd = c("LSD_MAKE", "LSDCODE", "MODELYR", "PAYEXPX", "PMTYEAR", "NUMPAY", "LSDENDYR", "QADDOWNX", "QADFEEX", "TRADEEXP"),

    rnt = c("SAMP_UN", "RTWATER", "RTTRASH", "RTINTRNT", "RTHEAT", "RTGAS", "RTELECT", "PUBLHOUS", "GOVTCOST")

    # ntaxi = c("AMTDEDCT", "CHLDCARE", "DEPCNT", "DEPUND13", "DEPUND17", "DEPUND18",
    #           "DIVINC", "FILESTAT", "FTAXOWE", "NONTXINC", "OTHDEDCT", "OTHTXINC", "PROPTXPD", "RNTPAID",
    #           "SOSSECB", "STAXOWE", "TAXPENS", "WAGE_HD", "WAGE_SP", "FDAGI_CY", "FDAGI_PY")

  )

  #-------------------------------

  for (survey_file in names(file.ids)) {

    ids <- file.ids[[survey_file]]

    # Get files paths to process
    fpath <- normalizePath(list.files(path = paste0("survey-raw/CEX/", survey_year), pattern = glob2rx(pattern = paste0(survey_file, "*.csv")), recursive = TRUE, full.names = TRUE))
    fpath <- fpath[!grepl("x.csv", fpath, fixed = TRUE)]  # Remove the "YRQx.csv" files, when applicable

    # Column names available in the input data
    dn <- names(data.table::fread(input = fpath[1], nrows = 1))

    # Desired variables that are actually available in input data
    select.vars <- intersect(file.vars[[survey_file]], dn)

    # Identify the "flag" variables
    flag.vars <- var_info %>%
      filter(file == toupper(survey_file), var %in% select.vars, !is.na(flag)) %>%
      select(var, flag) %>%
      distinct()

    # Crosswalk linking codes to code description, if applicable
    # vcodes <- var_info %>%
    #   filter(file == toupper(survey_file), var %in% select.vars) %>%
    #   select(file, var, var_desc, code, code_desc)

    # Read selected variables from file and set column type to "character"
    select.vector <- rep("character", length(ids) + length(select.vars) + length(flag.vars$flag))
    names(select.vector) <- c(ids, select.vars, flag.vars$flag)

    #-------------------------------

    getFileGeneric <- function(path) {

      dn <- names(data.table::fread(input = path, nrows = 1))
      sv <- select.vector[names(select.vector) %in% dn]
      d <- data.table::fread(input = path, select = sv, na.strings = c("", "."))
      d <- as_tibble(d)
      d <- mutate_if(d, is.character, na_if, y = "")  # Forces blank character strings to NA (not sure why fread isn't doing this automatically)

      # Rename the flag variables to a standard underscore suffix for easier identification
      i <- match(flag.vars$flag, names(d))
      names(d)[i] <- paste0(flag.vars$var, "_")

      # For APA and APB files: assign a unique ID for each appliance entry, by NEWID; necessary for call to spread()
      if (survey_file %in% c("apa", "apb")) {
        d <- d %>%
          group_by(NEWID) %>%
          mutate(APPID = 1:n()) %>%
          ungroup()
        ids <- c(ids, "APPID")
      }

      # For OVB file: detect/fixup any errors and restrict to Automobiles (100) or "Truck, van, minivan, or SUV" (110)
      if (survey_file %in% "ovb") {

        # Identify cases where NEWID-VEHICIB has duplicates (i.e. they do not uniquely identify each observation in OVB file)
        error.ovb <- d %>%
          group_by(NEWID, QYEAR, VEHICIB) %>%
          add_tally() %>%
          filter(n > 1) %>%
          select(-n)

        # If error exist, write flagged observations to disk and remove from 'd' before proceeding
        if (nrow(error.ovb) > 0) {
          message("OVB file: ", nrow(error.ovb), " obs. flagged and removed. See error log.")
          write_csv(error.ovb, paste0("survey-processed/CEX/errors/", Sys.Date(), " CEX OVB ", survey_year, " flagged observations", ".csv"))
          d <- d %>% anti_join(error.ovb)
        }

        # Restrict to Automobiles (100) or "Truck, van, minivan, or SUV" (110); Other codes includes things like motor homes, boats, etc.
        #d <- filter(d, VEHICYB %in% c(100, 110))

      }

      # For RNT file: retain a single response for each NEWID, preferring the response corresponding to the actual sampled housing unit
      if (survey_file %in% "rnt") {
        d <- d %>%
          arrange(NEWID, QYEAR, SAMP_UN) %>%
          group_by(NEWID) %>%
          slice(1) %>%
          ungroup()
      }

      # TURNED OFF: Insert vlaue labels in subsequent scripts
      # Create tibble with one row per NEWID entry
      # out <- d %>%
      #   gather(var, code, -all_of(ids)) %>%  # Note that 'code' contains the numeric response value when no lookup code is relevant for merge
      #   left_join(vcodes, by = c("var", "code")) %>%
      #   mutate(var = var,
      #          value = ifelse(is.na(code_desc), code, code_desc)) %>%
      #   select(all_of(ids), var, value) %>%
      #   spread(var, value) %>%
      #   mutate_all(type.convert, as.is = TRUE)

      # Assign "clean" state identifiers
      # This is necessary for safety, since the "STATE" codes do not use complete state name in some cases (e.g. "Massachuse")
      # if ("state" %in% names(d)) {
      #   state.data <- tidycensus::fips_codes %>%
      #     select(state, state_name) %>%
      #     distinct()
      #   asd <- adist(d$state, state.data$state_name)
      #   ind <- apply(asd, MARGIN = 1, FUN = function(x) ifelse(!anyNA(x), which.min(x), NA))
      #   d <- d %>%
      #     mutate(state = state.data$state[ind]) %>%
      #     as_tibble()
      # }

      return(d)

    }

    #-------------------------------

    # Apply getFileGeneric() to each individual file in 'fpath' and row-bind the results
    d <- map_dfr(fpath, getFileGeneric)

    #-------------------------------

    # Update 'd' to for flag variable values
    # This results in 'd' containing NA for genuine missing values and "Valid blank" for entries that should be updated with a plausible value
    # INFO on CEX missing data codes: https://www.bls.gov/cex/pumd-getting-started-guide.htm#section3
    for (v in names(d)) {
      x <- d[[v]]
      fv <- paste0(v, "_") # Flag variable (may not exist)
      f <- d[[fv]]  # Flag variable values
      if (!is.null(f)) {
        x[f %in% c("A", "H")] <- "Valid blank"  # Set valid blanks
        x[f %in% c("B", "C")] <- NA  # Set genuine NA's
        #[insert] Ignoring topcodes for now (could insert here; perhaps assign -Inf and Inf)
        d[[v]] <- x  # Update data column in 'd'
        d[[fv]] <- NULL  # Remove the flag variable column (no longer needed)
      }
    }

    #-------------------------------

    # Print message to console and save result to disk
    result <- d
    message("Processed ", survey_file, "_", survey_year, ": ", nrow(result), " rows; ", ncol(result), " columns.")
    saveRDS(result, paste0("survey-processed/CEX/CEI/", survey_year, "/", survey_file, "_", survey_year, ".rds"), compress = "bzip2")

  }

  #-------------------------------

  # Construct data dictionary containing variables in processed files
  # Note that this restricts the dictionary to variables specified in 'file.vars' and 'file.ids'

  # Restrict variable information to the files and variables requested
  vall <- c(unlist(file.ids), unlist(file.vars))
  vinfo <- var_info %>%
    filter(file %in% toupper(names(file.vars))) %>%
    filter(var %in% vall) %>%
    filter(survey_year >= first_year, is.na(last_year) | survey_year <= last_year)

  # SAFETY CHECK: Are all of the requested variables present in documentation after restricting to appropriate survey vintage?
  #temp <- filter(vinfo, survey_year >= first_year, is.na(last_year) | survey_year <= last_year)
  if (!all(vall %in% vinfo$var)) {
    warning("Not all variables detected in documentation for ", survey_year, " survey vintage.")
  }

  # Construct variable value-label crosswalk
  # Select entries that are consistent with the survey vintage and variables being processed
  # Success depends on accurate information in 'var_info'...
  vlabels <- vinfo %>%
    filter(!is.na(code)) %>%
    arrange(var, -first_year) %>%
    group_by(var, code) %>%
    slice(1) %>%
    select(var, code, code_desc) %>%
    rename(value = code, label = code_desc)

  # Extract variable descriptions (parsed for cleaner text)
  vdesc <- vinfo %>%
    mutate(desc = str_squish(map_chr(strsplit(var_desc, "|", fixed = TRUE), 1L))) %>%  # Remove text after "|" and trims whitespace
    select(var, desc) %>%
    arrange(var, desc) %>%
    group_by(var) %>%
    slice(1) %>%
    ungroup()

  # Assemble complete dictionary
  dict <- vdesc %>%
    left_join(vlabels, by = "var") %>%
    arrange(var)

  # Drop variables without description
  # This usually results from some oddity/error in the raw documentation (or the variable simply isn't available for the requested period)
  drop <- unique(filter(dict, is.na(desc))$var)
  if (length(drop) > 0) {
    dict <- filter(dict, !var %in% drop)
    warning("Removed following variables due to lack of description:\n", paste(drop, collapse = "/n"))
  }

  # Save dictionary data frame to disk
  saveRDS(dict, paste0("survey-processed/CEX/CEI/", survey_year, "/dictionary_", survey_year, ".rds"), compress = "bzip2")

}
