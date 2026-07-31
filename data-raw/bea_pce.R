# ==============================================================================
# DATA CREATION SCRIPT: BEA Personal Consumption Expenditures (PCE) Series
# ==============================================================================
# Purpose:
#   Retrieves, structures, and exports National and State-level Personal
#   Consumption Expenditure (PCE) datasets from the Bureau of Economic
#   Analysis (BEA) API.
#
# Role in package:
#   Generates two exported datasets saved to `/data`:
#     1. `BEA_pce_national.rda`: Detailed annual national-level consumption
#        expenditures (Table 2.4.5U) along with its complete hierarchical
#        parent/child structure.
#     2. `BEA_pce_state.rda`: Annual state-level expenditure breakdowns
#        (Table SAEXP1) mapped to equivalent national PCE series identifiers
#        and parent hierarchies.
#   These objects serve as baseline macro-economic target totals for micro-data
#   harmonization, downscaling, and expenditure modeling routines.
#
# Data Sources (BEA API):
#   - National: Dataset `NIUnderlyingDetail`, Table `U20405` (Table 2.4.5U)
#   - State: Dataset `Regional`, Table `SAEXP1`
#
# Processing Highlights:
#   - Hierarchical Parsing: Constructs `parent1`, `parent2`, ... lineage columns
#     derived from BEA dot-separated category paths.
#   - Deductions & Offsets: Re-signs expenditure items labeled with "Less:"
#     (and their child categories) to negative values to allow straight summation
#     across sub-categories to yield accurate totals.
# ==============================================================================

# Function to create:
# data/BEA_pce_national.rda
# data/BEA_pce_state.rda

createPCEseries <- function() {

  require(bea.R)
  require(dplyr)
  require(stringr)
  require(purrr)

  # BEA API key associated with user
  bea.key <- "9A199546-172E-49D5-BAAA-FF56C6D4E1FF"

  # Function to "clean" vector of PCE series descriptions;
  # i.e. remove unnecessary trailing parenthetical information
  cleanPCEdesc <- function(x) {
    out <- sapply(x, function(v) {
      y <- str_split(v, fixed("("))
      z <- rev(y[[1]])[1]
      if (str_sub(z, 1, 4) == "part" | str_sub(z, 1, 1) %in% 0:9) v <- sub(paste0("(", z), "", v, fixed = TRUE) else v
    })
    names(out) <- NULL
    return(str_squish(out))
  }

  # ============================================================================
  # SECTION 1: National PCE (Table 2.4.5U)
  # ============================================================================

  # 1.1 Retrieve Table 2.4.5U metadata and parse PCE series hierarchy
  d <- bea.R::beaSearch(searchTerm = "Table 2.4.5U", beaKey = bea.key) %>%
    filter(DatasetName == "NIUnderlyingDetail",
           TableID == 'U20405') %>%
    mutate(line = as.integer(LineNumber)) %>%
    arrange(line) %>%
    rename(path = Path,
           pce_series = SeriesCode,
           pce_desc = LineDescription) %>%
    select(line, pce_desc, pce_series, path) %>%
    mutate(pce_desc = cleanPCEdesc(pce_desc))

  # Safety check
  stopifnot(all(d$line == row(d)[,1]))

  # Manual fix: If any 'path' strings end with ".", add the 'line' value to the end (it appears to be cut off)
  # See line 235 for example. This has been reported to BEA.
  # This code should be safe if/when the issue is fixed in API.
  d <- d %>% mutate(path = ifelse(str_sub(path, start = -1L, end = -1L) == ".", paste0(path, line), path))

  # 1.2 Create parent hierarchy structure of pce_series
  x <- strsplit(d$path, ".", fixed = TRUE)
  k <- max(map_int(x, length))
  y <- x %>%
    map(rev) %>%
    map(as.integer) %>%
    map(~ d$pce_series[.x]) %>%
    map(~ replace(rep(NA, k), 1:length(.x), .x))

  # Assemble hierarchy into data frame
  # Note that first column is removed, since this merely replicates each line's own pce_series (no additional information)
  national.pce.hierarchy <- do.call(rbind, y)[, -1L] %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0("parent", 1:(k - 1)))

  # Safety check
  stopifnot(nrow(d) == nrow(national.pce.hierarchy))

  # 1.3 Get annual Table 2.4.5U data values (all available years)
  national.pce.values <- bea.R::beaGet(list('UserID' = bea.key ,
                                            'Method' = 'GetData',
                                            'datasetname' = 'NIUnderlyingDetail',
                                            'TableName' = 'U20405',
                                            'Frequency' = 'A',
                                            'Year' = 'ALL',
                                            'ResultFormat' = 'json')) %>%
    mutate(line = as.integer(LineNumber)) %>%
    select(line, starts_with("DataValue_")) %>%
    setNames(gsub("DataValue_", "", names(.)))

  # 1.4 Combine metadata, hierarchy, and annual expenditure values
  d <- d %>%
    select(-path) %>%  # Do not need to retain the path variable
    cbind(national.pce.hierarchy) %>%
    left_join(national.pce.values, by = "line")

  # 1.5 Convert "Less: " items and their sub-series to negative values
  # This ensures that total PCE across detailed categories sums to correct totals
  i <- which(grepl("Less:", d$pce_desc, fixed = TRUE))  # Series with "Less:" in description
  j <- which(apply(national.pce.hierarchy, MARGIN = 1, FUN = function(x) any(x %in% d$pce_series[i])))  # Child series of those in 'i'
  d <- d %>% mutate_if(is.double, ~ ifelse(line %in% c(i, j), -1 * ., .))

  # NOT USED: MANUAL EDIT
  # The "DMUSRC" line item is manually assigned as a sub-component of the "DADMRC" series
  # This is done, because museum and library admissions are not separate from movies, live entertainment, etc. in the CEX
  # It is necessary to treat all of these as belonging to a single parent series (i.e. DADMRC)
  # d <- d %>%
  #   mutate_if(is.double, ~ replace(., pce_series == "DADMRC", .[pce_series == "DADMRC"] + .[213])) %>%
  #   mutate(level = ifelse(line == 213, level[212], level))

  # 1.6 Save National PCE output to disk
  BEA_pce_national <- d %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001)

  save(BEA_pce_national, file = "data/BEA_pce_national.rda", compress = TRUE)

  # ============================================================================
  # SECTION 2: State PCE (Table SAEXP1)
  # ============================================================================

  # 2.1 Fetch State PCE LineCodes and map descriptions to National PCE series codes
  line.codes <- bea.R::beaParamVals(beaKey = bea.key, setName = "Regional", paramName = "LineCode")[[1]] %>%
    filter(substring(Desc, 1, 8) == "[SAEXP1]") %>%
    rename(LineCode = Key) %>%
    distinct() %>%
    mutate(pce_desc = trimws(gsub("[SAEXP1] Total personal consumption expenditures:", "", Desc, fixed = TRUE)),
           pce_desc = ifelse(pce_desc == "[SAEXP1] Total personal consumption expenditures", "Personal consumption expenditures", pce_desc)) %>%
    left_join(BEA_pce_national %>% select(pce_series, pce_desc), by = "pce_desc") %>%
    select(LineCode, pce_series, pce_desc) %>%
    arrange(as.integer(LineCode))

  # Safety check
  stopifnot(!any(is.na(line.codes$pce_series)))

  # 2.2 Construct state-level parent hierarchy by matching national hierarchy
  x <- BEA_pce_national %>%
    slice(match(line.codes$pce_series, BEA_pce_national$pce_series)) %>%
    select(pce_series, starts_with("parent")) %>%
    apply(MARGIN = 1, FUN = function(x) intersect(x, line.codes$pce_series))

  k <- max(map_int(x, length))
  y <- map(x, ~ replace(rep(NA, k), 1:length(.x), .x))

  state.pce.hierarchy <- do.call(rbind, y)[, -1L] %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0("parent", 1:(k - 1))) %>%
    mutate(pce_series = line.codes$pce_series)

  stopifnot(nrow(line.codes) == nrow(state.pce.hierarchy))

  # 2.3 Retrieve state expenditure data for all LineCodes across all available years
  d <- line.codes$LineCode %>%
    map(~ bea.R::beaGet(list('UserID' = bea.key ,
                             'Method' = 'GetData',
                             'datasetname' = 'Regional',
                             'TableName' = 'SAEXP1',
                             'LineCode' = .x,
                             'GeoFips' = 'STATE',
                             'Year' = 'ALL',
                             'ResultFormat' = 'json'))) %>%
    bind_rows() %>%
    mutate(LineCode = map_chr(strsplit(Code, "-", fixed = TRUE), 2L),
           line = as.integer(LineCode),
           state_fips = substring(GeoFips, 1, 2),
           state_name = GeoName) %>%
    filter(as.integer(state_fips) %in% 1:56) %>%
    left_join(line.codes, by = "LineCode") %>%
    left_join(state.pce.hierarchy, by = "pce_series") %>%
    select(line, state_fips, state_name, pce_desc, pce_series, starts_with("parent"), starts_with("DataValue_")) %>%
    setNames(gsub("DataValue_", "", names(.))) %>%
    arrange(state_fips, line)

  # Safety check (50 states + DC = 51)
  stopifnot(length(unique(d$state_fips)) == 51)

  # 2.4 Re-sign deduction items ("Less:") to negative
  # Applies to "Less: Receipts from sales of goods and services by nonprofit institutions" in State PCE
  BEA_pce_state <- d %>%
    mutate_if(is.double, ~ ifelse(grepl("Less:", d$pce_desc, fixed = TRUE), -1 * ., .)) %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001)

  # 2.5 Save State PCE output to disk
  save(BEA_pce_state, file = "data/BEA_pce_state.rda", compress = TRUE)

  # Print summary logs of created dataset objects
  message("BEA_pce_national.rda dimensions: ", paste(dim(BEA_pce_national), collapse = " x "), ". Most recent year: ", last(names(BEA_pce_national)))
  message("BEA_pce_state.rda dimensions: ", paste(dim(BEA_pce_state), collapse = " x "), ". Most recent year: ", last(names(BEA_pce_state)))

}

# Execute function to build and export data files
createPCEseries()
