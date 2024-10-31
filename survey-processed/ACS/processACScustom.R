library(tidyverse)
library(data.table)
source("R/utils.R")

processACScustom <- function(year) {

  # List of custom functions provided in /custom
  flist <- list.files("survey-processed/ACS/custom", pattern = ".R$")

  # Do processing for each custom function in 'flist'
  out <- lapply(flist, function(fun) {

    # Load the custom function and run if for 'year'
    source(file.path("survey-processed/ACS/custom", fun))
    f <- get(sub(".R$", "", fun))
    a <- names(formals(f))
    if (length(a) != 1 | a[1] != "year") stop("The custom function ", custom, "() must have 'year' as its lone argument", sep = "")
    cat("Processing custom function in", fun, "for year", year, "\n")
    out <- f(year = year)  # Apply custom function for 'year'
    out$year <- as.integer(year)  # Ensures 'year' is present in output
    if (!'hid' %in% names(out)) stop("Custom function must return 'hid' variable in result")
    out <- arrange(out, across(any_of(c('hid', 'pid')))) # Ensure consistent row ordering for checks below

    # Determine if function output is person- or household-level
    pfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_P_processed.fst"), recursive = TRUE, full.names = TRUE)
    p.hid <- fst::read_fst(pfile, columns = "hid")[[1]]
    if ('pid' %in% names(out) & identical(p.hid, out$hid)) {
      cat(" -- Treating output as person-level\n")
    } else {
      h.hid <- fst::read_fst(sub("_P_", "_H_", pfile), columns = "hid")[[1]]
      if (!'pid' %in% names(out) & all(h.hid %in% out$hid)) {
        cat(" -- Treating output as household-level\n")
        out <- filter(out, hid %in% h.hid)  # This restricts household output to occupied, non-GQ units
      } else {
        stop("Unable to determine if output is person- or household-level")
      }
    }

    # Structure final output and ensure variable labels are present
    cvars <- setdiff(names(out), c('year', 'hid', 'pid'))  # The custom variables
    labs <- labelled::var_label(out)[cvars]
    miss <- setdiff(cvars, names(labs))
    if (length(miss)) stop("The following custom variables are not defined/labelled:", paste(miss, collapse = ", "), "\n")
    out %>%
      mutate_if(is.factor, safeCharacters) %>%
      mutate_if(is.numeric, convertInteger, threshold = 1) %>%
      mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
      labelled::set_variable_labels(.labels = labs, .strict = FALSE) %>%
      arrange(across(any_of(c('hid', 'pid'))))

  })

  # Combine/merge variables across different custom functions
  # Recursive merge of person-level and household-level custom variables
  pi <- map_lgl(out, ~ "pid" %in% names(.x))
  p <- if (any(pi)) reduce(out[pi], left_join, by = c('year', 'hid', 'pid'), relationship = 'one-to-one', suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(p), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during person-level merge: ", paste(dupe, collapse = ", "))
  h <- if (any(!pi)) reduce(out[!pi], left_join, by = c('year', 'hid'), relationship = 'one-to-one',  suffix = rep('__dupe', 2)) else NULL
  dupe <- grep("__dupe$", names(h), value = TRUE)
  if (length(dupe)) stop("Duplicate custom variable names detected during household-level merge: ", paste(dupe, collapse = ", "))

  # For both household- and person-level output, safely modify the dictionary file and save custom microdata records to disk
  for (i in c('h', 'p')) {
    d <- get(i)
    if (!is.null(d)) {

      # Assemble dictionary
      cat("Creating dictionary\n")
      dict <- fusionData::createDictionary(data = d, survey = "ACS", vintage = year, respondent = toupper(i), custom = TRUE)

      # Append to existing dictionary.rds file
      cat("Saving dictionary to disk\n")
      fname <- file.path("survey-processed/ACS", year, paste("ACS", year, toupper(i), "dictionary.rds", sep = "_"))
      vars <- names(fst::fst(sub("dictionary.rds", "processed.fst", fname)))  # Non-custom variables in the processed ACS microdata
      readRDS(fname) %>%
        filter(variable %in% vars) %>%  # This ensures that only the non-custom variables are retained before appending the custom variables
        mutate(custom = FALSE) %>%  # Ensures the non-custom/original variables are marked as such
        bind_rows(dict) %>%
        arrange(variable) %>%
        saveRDS(file = fname)

      # Save processed microdata to disk (.fst)
      cat("Saving custom microdata to disk\n")
      fst::write_fst(x = d, path = sub("dictionary.rds", "custom.fst", fname), compress = 100)

    }
  }

}
