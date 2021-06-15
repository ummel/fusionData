# library(labelled)
# library(tidyverse)
# source("R/utils.R")

#-----

processFMLI <- function(survey_years) {

  # Replace this dependency, eventually!!!
  data(psu_info)

  # Load raw data for the 'file' in question
  d <- paste0("survey-processed/CEX/", survey_years, "/fmli_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(
      survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year
      cuid = as.integer(str_sub(NEWID, 1, -2)),
      intnum = as.integer(str_sub(NEWID, -1, -1))
    )

  #----------

  #hhid = ifelse(is.na(hhid), cuid, str_pad(hhid, width = 6, pad = 0)),  # THIS DOES NOT IDENTIFY AS EXPECTED; question sent to BLS
  #weight = FINLWT21 / mean(FINLWT21),  # Standardize the CU observation weights
  #urban = recode(bls_urbn, Yes = "Urban", No = "Rural"),
  #psu_type = ifelse(smsastat == "Yes", "CBSA", "non-CBSA"),
  #popsize = ifelse(psu_type == "non-CBSA", "unknown", popsize),
  # race_ref = recode(ref_race, `Native American` = "Other", `Pacific Islander` = "Other", `Multi-race` = "Multiracial"),  # Consolidate the 'race' variable; this is possible b/c there are no NA's in 'membrace' or 'horigin'
  # race_ref = ifelse(hisp_ref == "Hispanic", "Latino", race_ref),
  #swimpool = na_if(recode(swimpool_, D = "Yes", A = "No"), "C"), # Pre-process 'swimpool' so it is available for region-division imputation
  #psu = ifelse(is.na(psu), "Other", psu)) %>%  # Assume PSU name is valid for the self-representing MSA's and set to "Other" for all other observations
  #rename(psu_name = psu) %>%
  #select(-NEWID, -FINLWT21, -ref_race, -hisp_ref, -age_ref_, -ref_race_) %>%   # Drop the age_ref_ flag since we don't need/want to impute it

  # TEMPORARY until this gets sorted out with BLS (email to Scott Curtis about odd values)...
  #select(-hhid, -hhid_)

  #----------

  # FIX-UPS of known issues concerning geographic variable naming and state-division-region hierarchy
  # This section introduces 'state_name' and ensures the state-division-region hierarchy is consistent

  # Assign "state_name" variable by merging on the raw/original "STATE" FIPS code variable
  # This is necessary for safety, since the "STATE" variable labels do not use complete state name in some cases (e.g. "Massachuse")
  state.lookup <- tidycensus::fips_codes %>%
    select(state_code, state_name) %>%
    distinct()
  d <- left_join(d, state.lookup, by = c("STATE" = "state_code"))

  # TO DO: CHANGE: Use generic geo hierarhcy data -- not psu_info!
  # MANUAL FIX for known geographic inconsistencies (waiting for BLS to fix in original data)
  # See hierarchy here: https://www.bls.gov/cex/csxgeography.htm
  state.hierarchy <- psu_info %>%
    select(state_name, division, region) %>%
    distinct()
  d <- d %>%
    left_join(state.hierarchy, by = "state_name") %>%
    mutate(DIVISION = ifelse(!is.na(division), division, DIVISION),
           REGION = ifelse(!is.na(region), region, REGION),
           state_fips = STATE) %>%
    select(-STATE, -division, -region)

  # Check for basic geographic inconsistencies
  stopifnot(d %>% filter(!is.na(state_name), is.na(REGION) | is.na(DIVISION)) %>% nrow() == 0)
  stopifnot(d %>% filter(!is.na(DIVISION), is.na(REGION)) %>% nrow() == 0)

  # Safety check: Flag apparent violations of region-division-state hierarchy
  problems <- d %>%
    select(REGION, DIVISION, state_name) %>%
    rename_with(tolower) %>%
    distinct() %>%
    na.omit() %>%
    anti_join(psu_info %>% select(region, division, state_name) %>% distinct(), by = c("region", "division", "state_name"))
  stopifnot(nrow(problems) == 0)

  #----------

  # Convert all variable names to lower case for ease-of-use
  names(d) <- tolower(names(d))

  #----------

  # Add data from the 'rnt' file
  # These are primarily variables indicating if rent payment includes certain types of consumption (e.g. electricity)
  rnt <- paste0("survey-processed/CEX/", survey_years, "/rnt_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year
           cuid = as.integer(str_sub(NEWID, 1, -2)),
           intnum = as.integer(str_sub(NEWID, -1, -1))) %>%
    rename_with(tolower) %>%
    select(-newid, -qyear, -starts_with("samp_un"))

  # Add 'rnt' sourced variables to 'd'
  d <- d %>%
    left_join(rnt, by = c("survey_year", "cuid", "intnum")) %>%
    mutate_at(setdiff(names(rnt), c("survey_year", "cuid", "intnum")), ~ replace(.x, cutenure != 4, 2))   # This assigns "2" ('No') for cases where the household does NOT pay rent (cutenure = 4: "Rented")

  #----------------

  # Load codebook
  codebook <- paste0("survey-processed/CEX/", survey_years, "/dictionary_", survey_years, ".rds") %>%
    map_dfr(readRDS) %>%
    mutate(var = tolower(var)) %>%
    filter(var %in% names(d)) %>%
    group_by(var, value) %>%
    slice(1) %>%
    ungroup()

  # Clean up the codebook
  codebook <- codebook %>%
    mutate(

      # Manual edits to variable descriptions ('desc')
      desc = gsub("CU", "consumer unit", desc),
      desc = gsub("PSU", "Primary Sampling Unit", desc),
      desc = ifelse(var == "building", "Dwelling building description or type", desc),
      desc = ifelse(var == "built", "Year the property was built", desc),
      desc = ifelse(var == "cntralac", "Does this unit have central air conditioning?", desc),
      desc = ifelse(var == "windowac", "Does this unit have window air conditioning?", desc),
      desc = ifelse(var == "vehql", "Total number of leased vehicles", desc),
      desc = map_chr(strsplit(desc, ", mean of", fixed = TRUE), 1),
      desc = ifelse(grepl("replicate weight", desc), str_sub(desc, 1, 35), desc),
      desc = str_to_sentence(desc),

      # Manual edits to variable labels ('label')
      label = ifelse(var == "building", map_chr(strsplit(label, " (", fixed = TRUE), 1), label),
      label = ifelse(var == "building" & value == "10", "College dormitory", label),
      label = ifelse(var == "building" & value == "11", "Other", label),
      label = ifelse(var == "cntralac", "Yes", label),
      label = ifelse(var == "cutenure" & value == "1", "Homeowner with mortgage", label),
      label = ifelse(var == "cutenure" & value == "2", "Homeowner without mortgage", label),
      label = ifelse(var == "cutenure" & value == "3", NA, label),  # Force imputation ("Homeowner-Mortgage status not reported")
      label = ifelse(var == "cutenure" & value == "5", "Occupied without payment of cash rent", label),
      label = ifelse(var == "swimpool", "Yes", label),
      label = ifelse(var == "windowac", "Yes", label),
      label = ifelse(var == "unistrq" & value == "01", NA, label),  # Force imputation ("Only OTHER units")
      label = ifelse(label == "Honolulu, HI", "Urban Honolulu, HI", label),
      label = ifelse(grepl("unspecified", label, ignore.case = TRUE), NA, label)

    ) %>%

    # Add manual entries to the codebook
    add_row(var = "cuid", desc = "Consumer unit unique identifier", value = NA, label = NA) %>%
    add_row(var = "intnum", desc = "Interview number", value = NA, label = NA) %>%
    add_row(var = "state_name", desc = "State name", value = NA, label = NA) %>%
    add_row(var = "state_fips", desc = "State FIPS code", value = NA, label = NA) %>%
    add_row(var = "psu", desc = "Primary sampling unit", value = NA, label = "Other") %>% # Assume PSU name is valid for the self-representing MSA's and set to "Other" for all other observations
    mutate_all(str_squish)

  #----------------

  # Variables with "Valid blank" values
  # These are the variables for which suitable replacement values must be specified below
  na.vars <- d %>%
    map_lgl(~ any(.x == "Valid blank", na.rm = TRUE)) %>%
    which() %>%
    names()

  #----------------

  # Manually constructed...
  # What value should "Valid blank" take for the following variables?

  # Code to help discern valid blank values
  # table(d$hlfbathq)
  # View(filter(d, hlfbathq == "Valid blank"))
  # filter(codebook, var == "hlfbathq")

  na.values <- list(

    hhid = 0,
    bathrmq = "Unknown, student housing",
    bedroomq = "Unknown, student housing",
    built = NA,  # Decided to impute all unknown values
    hlfbathq = "Unknown, student housing",
    intrdvxm = 0,
    netrentm = 0,
    othregxm = 0,
    othrincm = 0,
    renteqvx = NA,  # ! This is updated separately in subsequent code (below)
    retsurvm = 0,
    roomsq = "Unknown, student housing",
    royestxm = 0,
    welfarem = 0,
    swimpool = "No",
    studfinx = 0,
    fmlpyyrx = 0,
    stockx = 0,
    stockb = NA,  # ! This is updated separately in subsequent code (below)
    liquidx = 0,
    liquidb = NA,  # ! This is updated separately in subsequent code (below)
    irax = 0,
    irab = NA,  # ! This is updated separately in subsequent code (below)
    windowac = "No",
    cntralac = "No",
    rtwater = "No",
    rttrash = "No",
    rtintrnt = "No",
    rtheat = "No",
    rtgas = "No",
    rtelect = "No",
    govtcost = "No, government is not paying part of housing cost"

  )

  # Safety check for missing entries in 'na.values'
  miss <- noquote(setdiff(na.vars, names(na.values)))
  stopifnot(length(miss) == 0)

  #----------------

  # Assign levels for ordered factors
  # In general, we want to coerce unordered factor to ordered factors whenever feasible
  # There is some judgment involved

  ordered.factors <- list(

    irab = c("$0 - $1999", "$2,000 - $9,999", "$10,000 - $49,999", "$50,000 - $199,999", "$200,000 - $449,999", "$450,000 and over"),
    liquidb = c("$0 - $499", "$500 - $999", "$1000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over"),
    popsize = c("Less than 100 thousand", "100-500 thousand", "0.5-1.0 million", "1-5 million", "More than 5 million"),
    stockb = c("$0 - $1999", "$2,000 - $9,999", "$10,000 - $49,999", "$50,000 - $199,999", "$200,000 - $449,999", "$450,000 and over"),
    unistrq = c("Mobile home or trailer", "One, detached", "One, attached", "2 housing units", "3-4 housing units", "5-9 housing units", "10-19 housing units", "20-49 housing units", "50 or more housing units"),
    roomsq = c("Unknown, student housing", 1:99)

  )

  # Safety check
  # Detect any variables in 'ordered.factors' that are NOT in the codebook
  extras <- noquote(setdiff(names(ordered.factors), codebook$var))
  stopifnot(length(extras) == 0)

  # Safety check
  # Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
  # Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
  for (v in names(ordered.factors)) {
    of <- sort(ordered.factors[[v]])
    cb <- sort(unique(filter(codebook, var == v)$label))
    if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
  }

  #----------------

  # Update variable values with associated labels from 'codebook'

  # Loop through each variable in 'd', assigning labels when applicable
  for (v in names(d)) {

    cb <- filter(codebook, var == v)
    x <- d[[v]]
    y <- unlist(cb$value)
    z <- unlist(cb$label)
    if (v %in% names(na.values)) {
      y <- c(y, "Valid blank")
      z <- c(z, na.values[[v]])
    }
    m <- match(x, y)
    new.labels <- z[na.omit(m)]

    # Update 'x' with new value labels
    x[!is.na(m)] <- new.labels

    # Coerce result to ordered factor, if specified
    # Note that levels are restricted to those actually present in the data
    if (v %in% names(ordered.factors)) {
      num.na <- sum(is.na(x))
      x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
      stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }

    # Apply type.convert() to 'x'; leave ordered factors unchanged
    x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

    # Ensure unordered factor levels are sorted alphabetically
    if (is.factor(x) & !is.ordered(x)) x <- factor(x, levels = sort(unique(x)))

    # Update column in 'd'
    d[[v]] <- x

  }

  # Safety check
  # Ensure there are not "Valid blank" entries remaining in the data
  stopifnot(!any(d == "Valid blank", na.rm = TRUE))

  #----------------

  # Check for misspelling of CBSA names (NOTE: relies on psu_info -- want to switch eventually)
  stopifnot(length(setdiff(setdiff(d$psu, psu_info$psu_name), "Other")) == 0)

  # # Safety check: Flag erroneous geographic entries
  # problems <- d %>%
  #   rename(psu_name = psu) %>%
  #   filter(!is.na(division), psu_name != "Other") %>%
  #   select(region, division, state, psu_name) %>%
  #   anti_join(psu_info, by = c("region", "division", "state", "psu_name")) %>%
  #   na.omit()
  # stopifnot(nrow(problems) == 0)
  #
  # # Safety check: Check that geographic variables are consistent within CU's
  # problems <- d %>%
  #   select(cuid, region, division, state, psu_name) %>%
  #   distinct() %>%
  #   add_count(cuid) %>%
  #   filter(n > 1)
  # stopifnot(nrow(problems) == 0)
  #
  # # Trap erroneous geographic entries
  # error.geo <- d0 %>%
  #   select(survey_year, cuid, intnum, qintrvmo, qintrvyr, region, division, state) %>%
  #   anti_join(d %>% select(cuid, region, division, state), by = c("cuid", "region", "division", "state"))
  #
  # # Write potential errors to disk
  # if (nrow(error.geo) > 0) write_csv(error.geo, paste0("survey-processed/CEX/errors/", Sys.Date(), " CEX state-division-region flagged", ".csv"))

  # Number of missing values for the region-division-state geographic variables
  # d %>%
  #   select(region, division, state) %>%
  #   map_int(~sum(is.na(.x)))

  #----------

  # Update 'renteqvx' variable to reflect estimated rent or rental-equivalence of dwelling (monthly value)
  # Replace NA values for 'renteqvx' (i.e. rented units) with actual rent paid ('totalrent')
  # Set 'renteqvx' to NA (to be imputed) when 'cutenure' indicates no cash rent is paid
  # Set 'renteqvx' to NA (to be imputed) when original value is below $100 per month
  # renteqvx: "If someone were to rent your home today, how much do you think it would rent for monthly, unfurnished and without utilities?"
  # rendwecq/rendwepq: Rented dwelling this quarter/Rented dwelling last quarter (i.e. quarterly total for rent paid)
  d <- d %>%
    mutate(renteqvx = ifelse(is.na(renteqvx), (rendwecq + rendwepq) / 3, renteqvx),  # Monthly rent equivalent
           renteqvx = ifelse(cutenure == "Occupied without payment of cash rent", NA, renteqvx),   # Sets rent-equivalent to zero for cases where the household rents without payment
           renteqvx = ifelse(renteqvx < 100, NA, renteqvx)) %>%   # Hard-coded minimum plausible value
    select(-rendwecq, -rendwepq)

  #----------------

  # Process variables that have both a continuous version (e.g. "liquidx") and a secondary "range" version (e.g. "liquidb")
  # By updating the range version to reflect reported continuous values, there is some additional value-added when imputing the continuous version
  # i.e. if respondent reports range but not a value, we know the imputed value must be within the specified range

  d <- d %>%
    mutate(

      liquidx = ifelse(!is.na(liquidb) & liquidx == 0, NA, liquidx),
      irax = ifelse(!is.na(irab) & irax == 0, NA, irax),
      stockx = ifelse(!is.na(stockb) & stockx == 0, NA, stockx),

      liquidb = replace(liquidb, is.finite(liquidx), levels(liquidb)[findInterval(liquidx[is.finite(liquidx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))]),
      irab = replace(irab, is.finite(irax), levels(irab)[findInterval(irax[is.finite(irax)], vec = c(0, 2000, 10e3, 50e3, 200e3, 450e3, Inf))]),
      stockb = replace(stockb, is.finite(stockx), levels(stockb)[findInterval(stockx[is.finite(stockx)], vec = c(0, 2000, 10e3, 50e3, 200e3, 450e3, Inf))]),

    )

  # Check...
  #View(d %>% select(irax, irab) %>% distinct())

  #-----

  return(d)

}
