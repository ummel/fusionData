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

  # Detext if file is .csv or .txt
  suffix <- str_sub(dictionary.file, start = -4)
  stopifnot(suffix %in% c(".csv", ".txt"))
  csv <- suffix == ".csv"

  # Either read the .csv file or pre-process the .txt file
  dictionary <- if (csv) {
    read.csv(dictionary.file, header = FALSE, na.strings = "") %>%
      setNames(c('record', 'var', 'type', 'length', 'value', 'value2', 'label')) %>%
      select(var, value, value2, label)
  } else {
    convertTXTdictionary(dictionary.file)
  }

  #----------

  codebook <- dictionary %>%

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

    filter(
      !(str_sub(var, 1, 3) == "RAC" & label %in% c("Yes", "No")),  # Remove Yes/No race recode variables, since race information captured in RAC1P
      !grepl("allocation flag", tolower(desc), fixed = TRUE),  # Remove allocation flag variables
      !grepl("eligibility coverage edit", desc, fixed = TRUE),  # Remove healt insurance coverage edit variables (not necessary)
      !grepl("See 'Employment Status Recode' (ESR)", desc, fixed = TRUE),  # Remove detailed employment questions since 'ESR' variable captures this information
      !grepl("^MLP.", var)  # Remove veteran period of service recodes (all information contained in 'VPS')
    ) %>%

    # Second attempt to remove allocation flag variables
    # This is necessary, because text formatting bugs in .txt dictionary files can result in the allocation flag text going to "value" instead of "desc"
    group_by(var) %>%
    mutate(alloc_flag = any(grepl("allocation flag", tolower(value), fixed = TRUE))) %>%
    ungroup() %>%
    filter(!alloc_flag) %>%

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
      desc = ifelse(var == "RMSP", "Number of rooms, excluding bathrooms", desc),  # Helpful for RMSP variable to note that it excludes bathrooms in room count
      desc = capFirst(desc)
    ) %>%

    mutate(
      label = ifelse(var == "ST", value, label), # Replace state text names with FIPS code
      label = gsub("/ ", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = gsub(" / ", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = gsub("//", "/", label, fixed = TRUE),  # Manual text error fix-ups
      label = ifelse(str_sub(label, 1, 3) == "N/A", parText(label), label),   # For N/A labels, replace with parenthetical text
      label = gsub(" FT", " full-time", label, fixed = TRUE),
      label = gsub("NILF ", "Not in labor force ", label, fixed = TRUE),
      label = gsub("<", "less than", label, fixed = TRUE),
      noedit = grepl("/[0-9]", label) | is.na(label),
      label = ifelse(noedit, label, map_chr(strsplit(label, split = "/"), ~ paste(capFirst(.x[!tolower(.x) %in% c("gq", "vacant")]), collapse = " / "))),
      noedit = NULL,
      label = ifelse(grepl("suppress", tolower(label)), NA, label),  # Set suppressed values to NA to be imputed
      label = sub("^\\.", "", label),  # Remove any leading periods
      label = capFirst(label)
    ) %>%

    add_count(var) %>%
    mutate(label = ifelse(n == 1 & is.na(value) & !is.na(label), 0, label)) %>%
    mutate_if(is.character, str_squish) %>%
    distinct() %>%   # Final check to eliminate duplicate entries
    select(var, desc, value, label, adj)

  return(codebook)

}

#------------------------

# Function to convert .txt dictionary file (pre-2017) to something similar to the .csv structure that is provided from 2017 onward

# Example input
#file <- "survey-raw/ACS/2015/PUMSDataDict15.txt"

convertTXTdictionary <- function(file) {

  d <- readLines(file)

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
#  d <- d[-k]

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
  ind <- which(map_lgl(d, ~ grepl("[A-Z]+", .x[1]) & length(.x) == 2 & !is.na(suppressWarnings(as.numeric(.x[2])))))

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
  return(result)

}
