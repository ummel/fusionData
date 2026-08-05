#' @rdname processACSmicrodata
#' @aliases NULL
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
