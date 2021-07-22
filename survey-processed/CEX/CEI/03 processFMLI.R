# Called from within compileCEI()

processFMLI <- function(survey_years, codebook) {

  # Load raw data for the 'file' in question
  d <- paste0("survey-processed/CEX/CEI/", survey_years, "/fmli_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(
      survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year
      cuid = as.integer(str_sub(NEWID, 1, -2)),
      intnum = as.integer(str_sub(NEWID, -1, -1))
    ) %>%
    rename_with(tolower) %>%
    filter(st_hous != 1, cutenure != 6) %>%  # Removes CU's explicitly identified as student housing, and those that are also classified as such by 'cutenure' (unclear why these two variables are not consistent)
    select(-newid, -hhid, -st_hous)

  #----------

  # Add data from the 'rnt' file
  # These are primarily variables indicating if rent payment includes certain types of consumption (e.g. electricity)
  rnt <- paste0("survey-processed/CEX/CEI/", survey_years, "/rnt_", survey_years, ".rds") %>%
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

  # Subset 'codebook' object in global environment to restrict to variables in 'd'

  codebook <- codebook %>%
    filter(var %in% names(d))

  #----------------

  # What value should "Valid blank" take for the following variables?

  # Variables with "Valid blank" values
  # These are the variables for which suitable replacement values must be specified below
  na.vars <- d %>%
    map_lgl(~ any(.x == "Valid blank", na.rm = TRUE)) %>%
    which() %>%
    names()

  # Code to help discern valid blank values
  # table(d$hlfbathq)
  # View(filter(d, hlfbathq == "Valid blank"))
  # filter(codebook, var == "hlfbathq")

  na.values <- list(

    # bathrmq = "Unknown, student housing",
    # bedroomq = "Unknown, student housing",
    # hlfbathq = "Unknown, student housing",
    roomsq = NA,  # Decided to impute all unknown values
    bathrmq = NA,  # Decided to impute all unknown values
    bedroomq = NA,  # Decided to impute all unknown values
    hlfbathq = NA,  # Decided to impute all unknown values
    roomsq = NA,  # Decided to impute all unknown values
    built = NA,  # Decided to impute all unknown values
    intrdvxm = 0,
    netrentm = 0,
    othregxm = 0,
    othrincm = 0,
    renteqvx = NA,  # ! This is updated separately in subsequent code (below)
    retsurvm = 0,
    royestxm = 0,
    welfarem = 0,
    swimpool = "No",
    fmlpyyrx = 0,
    studfinx = 0,
    studntx = 0,
    studntb = NA,  # ! This is updated separately in subsequent code (below)
    stockx = 0,
    stockb = NA,  # ! This is updated separately in subsequent code (below)
    liquidx = 0,
    liquidb = NA,  # ! This is updated separately in subsequent code (below)
    irax = 0,
    irab = NA,  # ! This is updated separately in subsequent code (below)
    othastx = 0,
    othastb = NA,  # ! This is updated separately in subsequent code (below)
    wholifx = 0,
    wholifb = NA,  # ! This is updated separately in subsequent code (below)
    credfinx = 0,
    creditx = 0,
    creditb = NA,  # ! This is updated separately in subsequent code (below)
    othfinx = 0,
    othlonx = 0,
    othlonb = NA,  # ! This is updated separately in subsequent code (below)
    windowac = "No",
    cntralac = "No",
    incnonw1 = "Reference person worked",
    incomey1 = "Reference person did not work",
    occucod1 = "Reference person did not work",
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

    educ_ref = c("Never attended", "Nursery, kindergarten, and elementary (grades 1-8)", "High school (grades 9-12), no degree", "High school graduate", "Some college, no degree", "Associate's degree in college", "Bachelors degree", "Masters, professional or doctorate degree"),
    popsize = c("Less than 100 thousand", "100-500 thousand", "0.5-1.0 million", "1-5 million", "More than 5 million"),
    unistrq = c("Mobile home or trailer", "One, detached", "One, attached", "2 housing units", "3-4 housing units", "5-9 housing units", "10-19 housing units", "20-49 housing units", "50 or more housing units"),
    irab = c("$0 - $1999", "$2,000 - $9,999", "$10,000 - $49,999", "$50,000 - $199,999", "$200,000 - $449,999", "$450,000 and over"),
    liquidb = c("$0 - $499", "$500 - $999", "$1,000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over"),
    stockb = c("$0 - $1999", "$2,000 - $9,999", "$10,000 - $49,999", "$50,000 - $199,999", "$200,000 - $449,999", "$450,000 and over"),
    studntb = c("$0 - $499", "$500 - $999", "$1,000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over"),
    othastb = c("$0 - $1999", "$2,000 - $9,999", "$10,000 - $49,999", "$50,000 - $199,999", "$200,000 - $449,999", "$450,000 and over"),
    wholifb = c("$0 - $499", "$500 - $999", "$1,000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over"),
    creditb = c("$0 - $499", "$500 - $999", "$1,000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over"),
    othlonb = c("$0 - $499", "$500 - $999", "$1,000 - $2,499", "$2,500 - $9,999", "$10,000 - $34,999", "$35,000 and over")

  )

  # Safety check
  # Detect any variables in 'ordered.factors' that are NOT in the codebook
  extras <- noquote(setdiff(names(ordered.factors), codebook$var))
  stopifnot(length(extras) == 0)

  # Safety check - RUN MANUALLY WHEN INTRODUCING NEW years/variables
  # Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
  # Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
  # for (v in names(ordered.factors)) {
  #   of <- sort(ordered.factors[[v]])
  #   cb <- sort(unique(filter(codebook, var == v)$label))
  #   if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
  # }

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

    # Ensure unordered factor levels are sorted according to codebook order of levels
    # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
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

    # Update column in 'd'
    d[[v]] <- x

  }

  # Safety check
  # Ensure there are not "Valid blank" entries remaining in the data
  stopifnot(!any(d == "Valid blank", na.rm = TRUE))

  #----------

  # FIX-UPS of known issues concerning geographic variable naming and state-division-region hierarchy
  # This section introduces 'state_name' and ensures the state-division-region hierarchy is consistent

  state.hierarchy <- "geo-processed/concordance/geo_concordance.fst" %>%
    read_fst(columns = c("state", "region", "division")) %>%
    distinct()

  # Safety check: Flag apparent violations of state-region-division hierarchy
  problems <- d %>%
    mutate(state = str_pad(state, width = 2, pad = "0")) %>%
    select(cuid, intnum, state, region, division) %>%
    na.omit() %>%
    mutate_all(tolower) %>%
    anti_join(mutate_all(state.hierarchy, tolower), by = c("state", "region", "division"))

  # Write potential errors to disk
  if (nrow(problems) > 0) write_csv(problems, paste0("survey-processed/CEX/errors/", Sys.Date(), " FMLI geography flagged", ".csv"))

  # Fix any geographic discrepancies in the state-division-region hierarchy
  d <- d %>%
    mutate(state = str_pad(state, width = 2, pad = "0")) %>%
    left_join(state.hierarchy, by = "state", suffix = c("", "2")) %>%
    mutate(division = ifelse(!is.na(division2), division2, as.character(division)),
           region = ifelse(!is.na(region2), region2, as.character(region))) %>%
    select(-division2, -region2) %>%
    left_join(state.hierarchy %>% select(division, region) %>% distinct(), by = "division", suffix = c("", "2")) %>%
    mutate(region = ifelse(!is.na(region2), region2, as.character(region))) %>%
    select(-region2)

  # Create the "concordance" geographic variables
  # i.e. create consistency with variables in "geo_concordance.fst"
  # Variables common to 'd' and 'geo_concordance.fst' are ultimately used by assignLocation()
  # NOTE that both 'cex_metro' and 'cex_cbsasize' modify original variables to classify Rural CU's as such explicitly
  #  This violates the observed data for a handful of observations, but is necessary to allow valid alignment with geo_concordance
  #  Specifically, we cannot know the population size of non-CBSA PSU's, so these cases are re-coded as "Rural" based on Urban/Rural variable
  # NOTE use of 'cbsa13'. BLS staff (Nix.Brian@bls.gov) said they used CBSA definitions/codes from "about 2012"
  d <- d %>%
    mutate(
      cbsa13 = str_pad(psu, width = 5, pad = 0),
      ur12 = substring(bls_urbn, 1, 1)  # Retain just "U" or "R"
      #cex_metro = ifelse(smsastat == "Yes" & ur12 == "U", "Metro", "Not metro"), # Either "Metro" or "Not metro"
      #popsize = rev(c("Less than 100 thousand", "100-500 thousand", "0.5-1.0 million", "1-5 million", "More than 5 million"))[as.integer(popsize)],
      #cex_cbsasize = ifelse(ur12 == "R" , "Rural", popsize)
    ) %>%  # Set rural areas to "Rural"; otherwise use CBSA population size classification
    select(-psu, -bls_urbn, -smsastat, -popsize)

  # Visual check on frequency of the resulting geographic intersections
  # test <- d %>%
  #   group_by(cbsa15, ur12, cex_metro, cex_cbsasize) %>%
  #   summarize(w = sum(finlwt21), .groups = "drop") %>%
  #   mutate(w = w / sum(w))

  #----------

  # Update 'renteqvx' variable to reflect estimated rent or rental-equivalence of dwelling (monthly value)
  # Replace NA values for 'renteqvx' (i.e. rented units) with actual rent paid ('rendwecq + rendwepq')
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
      studntx = ifelse(!is.na(studntb) & studntx == 0, NA, studntx),
      othastx = ifelse(!is.na(othastb) & othastx == 0, NA, othastx),
      wholifx = ifelse(!is.na(wholifb) & wholifx == 0, NA, wholifx),
      creditx = ifelse(!is.na(creditb) & creditx == 0, NA, creditx),
      othlonx = ifelse(!is.na(othlonb) & othlonx == 0, NA, othlonx),

      liquidb = replace(liquidb, is.finite(liquidx), levels(liquidb)[findInterval(liquidx[is.finite(liquidx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))]),
      irab = replace(irab, is.finite(irax), levels(irab)[findInterval(irax[is.finite(irax)], vec = c(0, 2000, 10e3, 50e3, 200e3, 450e3, Inf))]),
      stockb = replace(stockb, is.finite(stockx), levels(stockb)[findInterval(stockx[is.finite(stockx)], vec = c(0, 2000, 10e3, 50e3, 200e3, 450e3, Inf))]),
      studntb = replace(studntb, is.finite(studntx), levels(studntb)[findInterval(studntx[is.finite(studntx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))]),
      othastb = replace(othastb, is.finite(othastx), levels(othastb)[findInterval(othastx[is.finite(othastx)], vec = c(0, 2000, 10e3, 50e3, 200e3, 450e3, Inf))]),
      wholifb = replace(wholifb, is.finite(wholifx), levels(wholifb)[findInterval(wholifx[is.finite(wholifx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))]),
      creditb = replace(creditb, is.finite(creditx), levels(creditb)[findInterval(creditx[is.finite(creditx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))]),
      othlonb = replace(othlonb, is.finite(othlonx), levels(othlonb)[findInterval(othlonx[is.finite(othlonx)], vec = c(0, 500, 1000, 2500, 10e3, 35e3, Inf))])

    )

  # Check...
  #View(d %>% select(irax, irab) %>% distinct())

  #----------------

  # Set the half-sample replicate weights to zero if NA
  d <- d %>%
    mutate_at(vars(matches("^wtrep")), replace_na, replace = 0L)

  #----------------

  return(d)

}
