# Return pre-processed ACS codebook

# Example input
# dictionary.file <- "survey-raw/ACS/2015/PUMSDataDict15.txt"
# dictionary.file <- "survey-raw/ACS/2019/PUMS_Data_Dictionary_2019.csv"

processACScodebook <- function(dictionary.file) {

  # Function to capitalize first letter in string
  capFirst <- function(x) {
    str_sub(x, 1, 1) <- toupper(str_sub(x, 1, 1))
    return(x)
  }

  # Function to return text found after the first open parenthesis
  parText <- function(x) {
    x <- sub(".*\\(", "", x)  # Remove everything up t0 and including first "("
    i <- which(str_sub(x, -1) == ")")
    x[i] <- str_sub(x[i], 1, -2)  # Remove ending ")", if present
    return(x)
  }

  #----------

  # Detect if the original/native dictionary file is .csv, .txt, or .pdf
  suffix <- str_sub(dictionary.file, start = -4)
  stopifnot(suffix %in% c(".csv", ".txt", ".pdf"))

  # Either read the .csv file or pre-process the .txt file
  ddata <- if (suffix == ".csv") {
    read.csv(dictionary.file, header = FALSE, na.strings = "") %>%
      setNames(c('record', 'var', 'type', 'length', 'value', 'value2', 'label')) %>%
      select(var, value, value2, label)
  } else {
    if (suffix == ".txt") {
      convertTXTdictionary(dictionary.file)
    } else {
      # When file is PDF, must convert to text first and then pass result to convertTXTdictionary()
      x <- pdftools::pdf_text(dictionary.file)
      x <- trimws(strsplit(paste(x, collapse = ""), split = "\n")[[1]])
      convertTXTdictionary(x)
    }
  }

  #----------

  codebook <- ddata %>%

    distinct() %>%   # Eliminate duplicate entries (i.e. one each for household and person-records)

    split(f = .$var) %>%

    map(~ mutate(.x,
                 rng = (value != value2 & !is.na(value2)),
                 miss = grepl("^(b)\\1*$", value),  # Matches "b", "bb", etc.
                 value = ifelse(miss, NA, value),  # Replace "b", "bb", etc. with NA
                 asis = !rng & !miss)) %>%

    map_dfr(~ tibble(var = .x$var[1],
                     desc = .x$value[1],
                     value = if (all(.x$asis)) {.x$value[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$value[.x$miss], if (any(.x$rng)) {NULL} else {.x$value[!.x$miss][-1]})}},
                     label = if (all(.x$asis)) {.x$label[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$label[.x$miss], if (any(.x$rng)) {NULL} else {.x$label[!.x$miss][-1]})}})) %>%

    mutate_if(is.character, str_squish) %>%  # Remove any unnecessary white space
    filter(
      !(str_sub(var, 1, 3) == "RAC" & label %in% c("Yes", "No")),  # Remove Yes/No race recode variables, since race information captured in RAC1P
      !grepl("allocation flag", tolower(desc), fixed = TRUE),  # Remove allocation flag variables (after 2008)
      !grepl("allocation$", tolower(desc)),  # Remove allocation flag variables (2008 and earlier)
      !grepl("eligibility coverage edit", desc, fixed = TRUE),  # Remove health insurance coverage edit variables (not necessary)
      !grepl("See 'Employment Status Recode' (ESR)", desc, fixed = TRUE),  # Remove detailed employment questions since 'ESR' variable captures this information
      !grepl("^MLP.", var)  # Remove veteran period of service recodes (all information contained in 'VPS')
    ) %>%

    # Second attempt to remove allocation flag variables
    # This is necessary, because text formatting bugs in .txt dictionary files can result in the allocation flag text going to "value" instead of "desc"
    group_by(var) %>%
    mutate(alloc_flag1 = any(grepl("allocation flag", tolower(value), fixed = TRUE)), # After 2008
           alloc_flag2 = any(grepl("allocation$", tolower(value)))) %>%  # 2008 and earlier
    ungroup() %>%
    filter(!alloc_flag1, !alloc_flag2) %>%

    # Second attempt to remove race recode variables
    # This is necessary, because text formatting bugs in .txt dictionary files can result in the race recode text going to "value" instead of "desc"
    group_by(var) %>%
    mutate(recode_flag = str_sub(var[1], 1, 3) == "RAC" & any(grepl("combination", paste(desc, value), fixed = TRUE))) %>%
    ungroup() %>%
    filter(!recode_flag) %>%

    mutate(
      temp = suppressWarnings(as.integer(value)),
      value = ifelse(is.na(temp), value, as.character(temp)),  # Coerces 'value' to integer (but stored as character) whenever possible in order to match raw data resulting from fread()
      temp = NULL,
      adj = ifelse(grepl("use adjhsg", tolower(desc)), "ADJHSG", NA),
      adj = ifelse(grepl("use adjinc", tolower(desc)), "ADJINC", adj)
    ) %>%

    mutate(
      desc = gsub("\\s*\\([^\\)]+\\)", "", desc),  # Remove parenthetical text in description
      desc = gsub("recode", "", desc),  # Remove word 'recode' as it doesn't seem necessary (might induce confusion)
      desc = gsub("HH", "household", desc),  # Replace 'HH' with 'household' to avoid confusion
      desc = ifelse(var %in% c("RMS", "RMSP"), "Number of rooms, excluding bathrooms", desc),  # Helpful for RMS/RMSP variable to note that it excludes bathrooms in room count
      desc = gsub("write-in", "", desc, fixed = TRUE),  # Remove "write-in" (unnecessary); appears to only affect year of naturalization variable
      desc = gsub("english", "English", desc),  # Upper case for English language
      desc = ifelse(desc == "VA", "VA health care", desc),  # Clearer than "VA" only
      desc = ifelse(desc == "Indian health service", "Indian Health Service", desc),
      desc = gsub(" puma ", " PUMA ", desc),
      desc = gsub(" soc codes", " SOC codes", desc),
      desc = gsub(" naics codes", " NAICS codes", desc),
      desc = gsub(" ind codes", " IND codes", desc),
      desc = capFirst(desc)
    ) %>%

    mutate(
      label = ifelse(var == "ST", value, label), # Replace state text names with FIPS code
      label = gsub("/ ", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = gsub(" / ", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = gsub("//", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = ifelse(str_sub(label, 1, 3) == "N/A", parText(label), label),   # For N/A labels, replace with parenthetical text
      label = gsub(" FT", " full-time", label, fixed = TRUE),
      label = gsub("NILF ", "Not-in-labor-force ", label, fixed = TRUE),
      label = gsub("<", "less than", label, fixed = TRUE),
      noedit = grepl("/[0-9]", label) | is.na(label),
      label = ifelse(noedit, label, map_chr(strsplit(label, split = "/"), ~ paste(capFirst(.x[!tolower(.x) %in% c("gq", "vacant")]), collapse = " / "))),
      noedit = NULL,
      label = ifelse(grepl("suppress", tolower(label)), NA, label),  # Set suppressed values to NA to be imputed
      label = sub("^\\.", "", label),  # Remove any leading periods
      label = capFirst(label)
    ) %>%

    # Manual removal of variables with unnecessary or redundant information
    filter(!var %in% c("RT", "DECADE", "SRNT", "SVAL", "OCPIP", "GRPIP", "DRIVESP", "DRATX", "SPORDER", "WAOB", "MRGX", "SMX")) %>%

    # Known, manual fix-ups to codebook that appear to be relevant across survey years
    mutate(
      label = ifelse(is.na(value) & var %in% c("BROADBND", "DIALUP", "HISPEED", "DSL", "FIBEROP", "MODEM", "OTHSVCEX", "SATELLITE"), "No paid access to the internet", label),
      label = ifelse(is.na(value) & var == "CPLT", "No couple present", label),  # Manual edit: codebook appears to be wrong
      label = ifelse(is.na(value) & var == "RNTM", "No", label),  # Non-renting units cannot have meals included in rent
      label = ifelse(is.na(value) & var == "LANP", "GQ/Vacant", label)  # Original label dropped because it is in parentheses
    ) %>%

    add_count(var) %>%
    mutate(label = ifelse(n == 1 & is.na(value) & !is.na(label), 0, label)) %>%
    mutate_if(is.character, str_squish) %>%
    distinct() %>%   # Final check to eliminate duplicate entries
    select(var, desc, value, label, adj)

  #-----

  # # Universal renaming of specific variables across ACS vintages
  # # Convert instances of newer variable name (e.g. TAXAMT) in 'codebook' to the earlier name (TAXP) for consistency over time
  # # Should be limited to cases where the underlying concept/question is the same; OK if the factor levels differ across vintages
  # # Prior to 2018, the property tax variable was "TAXP". More recent name is "TAXAMT".
  # # Prior to 2008, the property value variable was "VAL". More recent name is "VALP".
  # # Prior to 2020, the HU/GQ identifier "TYPE". More recent name is "TYPEHUGQ".
  # # Prior to 2020, the internet access was "ACCESS". More recent name is "ACCESSINET".
  # # Prior to 2021, the year built was "YBL". More recent name is "YRBLT".
  # varnames <- list(
  #   TAXAMT = "TAXP",
  #   VALP = "VAL",
  #   TYPEHUGQ = "TYPE",  # The TYPE variable is eventually dropped since there is no variation; renaming retained for consistency
  #   ACCESSINET = "ACCESS",
  #   YRBLT = "YBL"
  # )
  #
  # codebook$var0 <- NA  # 'var0' retains the original name of the variable so that it can be changed in the microdata later
  # for (v in names(varnames)) {
  #   i <- which(codebook$var == v)
  #   codebook$var0[i] <- v
  #   codebook$var[i] <- varnames[[v]]
  # }

  #-----

  # Known manual edits to ACS variable descriptions/definitions

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
    GRNTP = "Gross rent including utilites, monthly",  # https://www.census.gov/quickfacts/fact/note/US/HSG860221
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

  #-----

  return(codebook)

}

#------------------------

# Function to convert .txt dictionary file (pre-2017) to something similar to the .csv structure that is provided from 2017 onward

# Example input
#file <- "survey-raw/ACS/2016/PUMSDataDict16.txt"

convertTXTdictionary <- function(input) {

  if (file.exists(input[[1]])) {
    d <- readLines(input, warn = FALSE)
  } else {
    d <- input
  }

  # First line (guess start point based on "RT" variable being first)
  start <- which(substring(d, 1, 2) == "RT")[1]

  # Likely final line
  finish <- which(substring(d, 1, 7) == "PWGTP80") + 2L

  d <- d[start:finish]

  #-----

  # Strings that should be converted to single quotes
  quote.chars <- c("\x92", "\x93", "\x94")
  for (x in quote.chars) d <- gsub(x, "'", d, fixed = TRUE, useBytes = TRUE)

  # Manual fix-up for invalid character
  d <- gsub("\xa0", "", d, fixed = TRUE, useBytes = TRUE)

  # Replace with dash
  d <- gsub("\x96", "-", d, fixed = TRUE, useBytes = TRUE)

  # Remove asterisks
  #d <- gsub("\\*", "", d, fixed = TRUE)  # This doesn't catch all cases for some reason
  d <- gsub("\\*+", "", d)

  # Manual fix-up for some military occupation codes that run together, preventing appropriate string split
  mil.chars <- paste0("928110P", 1:7)
  for (x in mil.chars) d <- sub(x, paste0(x, " "), d, fixed = TRUE)

  # Replace an erroneous tab (see d[142])
  d <- gsub("$\t", "$", d, fixed = TRUE)

  # Replace tabs with pipe (|) that we will use later to split text strings into pieces
  d <- gsub("\t", "|", d, fixed = TRUE)

  # Remove pipes found at end of a line
  d <- sub("\\|$", "", d)

  # Insert extra space in front of ".N/A" to ensure all instances are split properly (see NAICSP variable)
  d <- sub(".N/A", " .N/A", d, fixed = TRUE)

  #-----

  # Identify which lines have orphaned text; append the orphaned text to end of previous line; remove orphans
  temp <- trimws(gsub("|", "", d, fixed = TRUE))
  k <- which(substring(temp, 1, 1) == ".")
  d[k] <- substring(temp[k], first = 2)
  for (i in rev(k)) d[i - 1] <- paste(d[i - 1], d[i])
  if (length(k)) d <- d[-k]

  #-----

  # Replace " ." with pipe so it can be split as well
  # Can't do this before orphans or we won't catch all of the legit orphans
  d <- gsub(" .", "|", d, fixed = TRUE)

  # Remove unnecessary spaces
  d <- stringr::str_squish(d)

  # Remove intentional blank lines
  d <- d[!grepl("intentionally blank", d, fixed = TRUE)]

  # Remove line identifying start of person record variables
  d <- d[!grepl("PERSON RECORD", d, fixed = TRUE)]

  #-----

  d <- lapply(d, strsplit, split = "\\|+")

  d <- map(d, ~ .x[[1]][str_squish(.x[[1]]) != ""])

  #-----

  # Identify indices in 'd' that are the start of a variable entry
  # This works for the codebook structure in some years, but not all
  ind <- which(map_lgl(d, ~ grepl("[A-Z]+", .x[1]) & length(.x) == 2 & !is.na(suppressWarnings(as.numeric(.x[2])))))

  # An alternative approach if the first call to 'ind' does not return any indices
  if (!length(ind)) {
    blanks <- which(lengths(d) == 0)  # Blank lines
    capletter <- which(map_lgl(d, ~ grepl("^[A-Z]+", .x[1])))  # Lines starting with a capital letter
    capword <- which(map_lgl(d, ~ word(.x[1], start = 1, end = 1) == toupper(word(.x[1], start = 1, end = 1))))  # Lines starting with an all-caps word
    ind <- intersect(intersect(blanks + 1, capletter), capword)  # Assumes new variable entry denoted by an all-caps word following a blank line
    ind <- setdiff(ind, blanks - 1)  # Removes single rows of capitalized words with a following blank
  }

  #-----

  # Function to parse text within each variable entry
  #i <- 21  # CONP
  #i <- 14  # AGS
  #i <- 43  # RNTP
  parseEntry <- function(i) {

    # Extract the lines in 'd' associated with variable entry number 'i'
    if (i == length(ind)) {
      x <- d[ind[i]:length(d)]
    } else {
      x <- d[ind[i]:(ind[i + 1] - 1)]
    }

    x <- compact(x)

    note <- map_lgl(x, ~ tolower(substring(.x[1], 1, 5)) == "note:")

    if (any(note)) {
      note.text <- paste(unlist(x[which(note):length(x)]), collapse = " ")
      note.text <- paste0("(", str_squish(substring(note.text, first = 6)), ")")
      drop <- c(1, 2, which(note):length(x))
    } else {
      note.text <- NULL
      drop <- c(1, 2)
    }

    y <- x[-drop]

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

  # For troubleshooting parseEntry()
  #for (i in seq_along(ind)) test <- parseEntry(i)

  # Parse all variables entries and assemble as data frame
  result <- map_dfr(seq_along(ind), parseEntry)

  # For 'var' (variable name) column; extract only the first word (the upper case variable identifier)
  result$var <- word(result$var, start = 1, end = 1)

  return(result)

}
