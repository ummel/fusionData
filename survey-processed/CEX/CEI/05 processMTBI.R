# Called from within compileCEI()
# base.year: Year to which expenditure values are deflated/inflated; i.e. the baseline year for dollar amounts

processMTBI <- function(survey_years, base.year) {

  load("survey-processed/CEX/ucc_assignment.rda")

  # Load categories data, sot that only the necessary UCC's are loaded from MTBI files on disk
  cats <- ucc_assignment %>%
    filter(file == "MTBI")

  #---

  # Load CPI data and create 'cpi' variable to inflate dollar values to final survey year
  data(cpi_series, package = "fusionData")
  cpi.adj <- cpi_series %>%
    mutate(base = mean(filter(., year == !!base.year)$cpi),
           cpi = base / cpi) %>%
    select(year, month, cpi)

  #---

  readMTBI <- function(survey_year) {

    # Get files paths to process
    fpath <- normalizePath(list.files(path = paste0("survey-raw/CEX/", survey_year), pattern = glob2rx(pattern = "mtbi*.csv"), recursive = TRUE, full.names = TRUE))
    fpath <- fpath[!grepl("x.csv", fpath, fixed = TRUE)]  # Remove the "YRQx.csv" files, when applicable

    # Read selected variables from file
    select.vars <- c("EXPNAME", "COST", "COST_", "UCC")
    select.vector <- rep("character", length(select.vars) + 3)
    names(select.vector) <- c("NEWID", "REF_MO", "REF_YR", select.vars)

    # Unique CU/calendar month/UCC combinations in output are identified by file-cuid-intnum-month
    # Column 'file' is an integer identifying the file in 'fpath' from which the data was obtained
    # !!! NOTE: MTBI does not appear to contain any COST_ flags that are "A", "B", or "C" (i.e. blanks); the only flag values are for valid entries or top/botcoded
    d <- fpath %>%
      map_dfr(data.table::fread, .id = "file", select = select.vector, na.strings = "", data.table = FALSE) %>%
      rename(expname = EXPNAME, ucc = UCC, cost = COST) %>%
      filter(ucc %in% cats$ucc) %>%  # Restrict to UCC's we are interested in...
      mutate(cuid = as.integer(str_sub(NEWID, 1, -2)),
             intnum = as.integer(str_sub(NEWID, -1, -1)),
             month = as.integer(REF_MO),
             year = as.integer(REF_YR),
             cost = as.numeric(cost),
             coded = COST_ %in% c("T", "U", "V", "W")) %>%  # These are bot/topcode flag values (there are no regular NA's in the MTBI data)
      group_by(cuid, intnum) %>%
      ungroup() %>%
      select(file, cuid, intnum, month, year, ucc, expname, coded, cost)

    return(d)

  }

  #-----

  # Load expenditure data across multiple survey years
  d <- survey_years %>%
    map_dfr(readMTBI, .id = "survey_year") %>%
    mutate(survey_year = survey_years[as.integer(survey_year)])  # Replace index value with actual survey year

  # NOTE: Variable 'expname' is "Name of expense variable from which UCC mapped" (UCC = "Universal Classification Code")
  # i.e. The relationship between 'ucc' and 'expname' is potentially many-to-many
  # About 60% of 'expname' values map to a single 'ucc'
  # About 80% of 'ucc' values map to a single 'expname'
  # The UCC value is what is ultimately used to map to a category (via 'cats')

  #-----

  # Convert "cost" to positive value for the repaid principal expenditure UCC's (they are negative by default)
  # The repaid principal categories are hard-coded as: "MRTGPP", "MRTGPS", and "VEHPRN"
  d <- d %>%
    mutate(cost = ifelse(ucc %in% filter(cats, cat %in% c("MRTGPP", "MRTGPS", "VEHPRN"))$ucc, abs(cost), cost))

  # check <- d %>%
  #   filter(ucc == "910050") %>%
  #   group_by(cuid, intnum) %>%
  #   filter(intnum == max(intnum)) %>%
  #   summarize(cost = sum(cost))

  # # Owner-occupied property value
  # ownval <- d %>%
  #   filter(ucc %in% filter(cats, cat == "OWNVAL")$ucc) %>%
  #   group_by(cuid, intnum) %>%
  #   filter(intnum == max(intnum)) %>%
  #   summarize(OWNVAL = 12 * mean(cost), .groups = "drop")
  #
  # # CAN I EXTRACT THIS OR NOT?
  # # Owner-occupied rental value
  # test <- d %>%
  #   filter(ucc %in% filter(cats, cat == "OWNRNT")$ucc) %>%
  #   group_by(cuid, intnum) %>%
  #   filter(intnum == max(intnum)) %>%
  #   summarize(OWNVAL = 12 * mean(cost), .groups = "drop")

  #-----

  # Aggregate expenditure data by primary consumption category ('cat')
  # The use of dense_rank() ensures that the expenditure columns are sorted by the sequence of available interviews
  # Example: If a 'cuid' has only their final interview in 'd' (intnum = 5), intnum is reassigned to '1' by dense_rank()
  d <- d %>%
    left_join(cpi.adj, by = c("year", "month")) %>%
    mutate(cost = cost * cpi) %>%   # Adjust for inflation
    left_join(cats %>% select(ucc, cat, ucc_share), by = "ucc") %>%
    mutate(cost = cost * ucc_share) %>%  # Multiply raw/original cost by 'ucc_share' to allocate (or adjust) as necessary
    group_by(cuid) %>%
    mutate(intrank = dense_rank(intnum)) %>%  # Add dense-ranked version of 'intnum'
    group_by(cuid, intnum) %>%
    mutate(emonth = weighted.mean(year + month / 12, pmax(0, cost))) %>%  # Assign single reference time to each cuid-intnum combination
    group_by(cuid, intnum, intrank, emonth, cat) %>%
    summarize(cost = sum(cost), .groups = 'drop')   # Aggregate expenditures at CU-level in preparation for making data wide (below)

  # Safety check
  stopifnot(!anyNA(d$cat))

  #-----

  # Spread the expenditure data wide
  # 'intrank' refers to the CU's "interview rank" (_1 for their first available interview, etc.)
  exp.intrank <- d %>%
    complete(nesting(cuid, intnum, intrank, emonth), cat, fill = list(cost = 0)) %>%
    pivot_wider(id_cols = cuid,
                names_from = c(cat, intrank), names_sep = "_",
                values_from = cost, values_fill = list(cost = NA))

  # See how many NA's there are by column
  # There should be none for *_1 and increasing amounts up to *_4
  #colSums(is.na(exp.intrank))
  stopifnot(!is.unsorted(colSums(is.na(exp.intrank))))

  #-----

  # Create 'year.intrank' and 'month.intrank' df's
  # These provide cuid-specified 'year_i' and 'month_i' columns providing time stamps for each expenditure period
  # These variables are available as predictors when imputing missing values

  temp <- d %>%
    select(-cat, -cost, -intnum) %>%
    distinct() %>%
    complete(cuid, intrank, fill = list(emonth = NA)) %>%
    group_by(cuid) %>%
    mutate(emonth = replace(emonth, is.na(emonth), max(emonth, na.rm = TRUE) + 0.25 * 1:sum(is.na(emonth)))) %>%
    ungroup() %>%
    mutate(year = as.integer(emonth),
           month = factor(as.integer(1 + floor(12 * (emonth - year))), levels = 1:12, ordered = TRUE),
           year = factor(year, levels = sort(unique(year)), ordered = TRUE))

  year.intrank <- pivot_wider(temp, id_cols = cuid, names_from = intrank, names_prefix = "year_", values_from = year)
  month.intrank <- pivot_wider(temp, id_cols = cuid, names_from = intrank, names_prefix = "month_", values_from = month)

  #-----

  # Assemble CU-level expenditures formatted for imputation
  expend <- year.intrank %>%
    inner_join(month.intrank, by = "cuid") %>%
    inner_join(exp.intrank, by = "cuid")

  return(expend)

}
