#' Convey an existing harmony file to new survey(s)
#'
#' @description
#' Attempts to convey harmonies in an existing harmony file to a new harmony file for different survey(s). This is useful when introducing a new vintage of an existing survey, and some of the already-specified harmonies may be valid for the new vintage.
#'
#' @param from Character. Name of the .R harmony file with existing harmonies of interest (e.g. "RECS_2015__ACS_2019.R")
#' @param to Character. Name of desired new harmony file (e.g. "RECS_2015__ACS_2015.R").
#' @param overwrite Logical. If \code{to} already exists, should it be overwritten? Default is \code{FALSE}.
#'
#' @details For a harmony in \code{from} to be considered valid for \code{to}, it must match exactly on variable names and all factor levels (if present). User should always manually check the \code{to} .R file before using it for analysis!
#'
#' @return The \code{to} harmony file is written to disk (at \code{/harmony/harmonies}), if valid harmonies are detected. The list object with retained harmonies is written to disk using internal function at \code{harmony/R/harmony2dotR.R}. Possible message printed to console indicating harmonies that are not strictly valid but should be explored manually via \link{harmony} app.
#'
#' @examples
#' conveyHarmony(from = "RECS_2015__ACS_2019.R", to = "RECS_2015__ACS_2015.R")
#'
#' @export

#-----

# Example input
# from <- "RECS_2015__ACS_2019.R"
# to <- "RECS_2015__ACS_2015.R"
# overwrite = FALSE

#-----

conveyHarmony <- function(from, to, overwrite = FALSE) {

  stopifnot(exprs = {
    from != to
    str_sub(from, -2, -1) == ".R"
    str_sub(to, -2, -1) == ".R"
    is.logical(overwrite)
  })

  # Load necessary function to write harmony file to disk
  source("harmony/R/harmony2dotR.R")

  # Function to split strings with double-underscore separator
  splitNames <- function(x) strsplit(x, "__", fixed = TRUE)

  # Function to extract clean levels from dictionary 'Values'
  clean <- function(x) gsub("[", "", gsub("]", "", str_squish(unlist(strsplit(x, split = "], ", fixed = T))), fixed = T), fixed = T)

  # Load survey dictionary
  data(dictionary, package = "fusionData")

  #-----

  # Names of donor and recipient variables in each "from" harmony
  hfile <- dget(paste0("harmony/harmonies/", from))
  hnames <- names(hfile)
  d.from <- map_chr(splitNames(hnames), 1)
  r.from <- map_chr(splitNames(hnames), 2)

  # Check if 'to' harmony file already exists
  file.out <- paste0("harmony/harmonies/", to)
  if (!overwrite & file.exists(file.out)) stop("The 'to' harmony file already exists; must set 'overwrite = TRUE'")

  # Get dictionary entries for the "to" surveys
  to.surveys <- gsub(".R", "", unlist(splitNames(to)), fixed = TRUE)
  d.dict <- filter(dictionary, paste(Survey, Vintage, sep = "_") %in% to.surveys[1])
  r.dict <- filter(dictionary, paste(Survey, Vintage, sep = "_") %in% to.surveys[2])

  #-----

  # Results placeholders
  keep <- NULL
  look <- NULL

  # Loop through each prospective harmony in 'hfile'
  for (i in 1:length(hfile)) {

    H <- hfile[[i]]

    dv <- d.from[i]
    ok1 <- dv %in% d.dict$Variable
    x <- sort(H[[1]]$levels)
    y <- sort(clean(filter(d.dict, Variable == dv)$Values))
    ok2 <- ifelse(length(x) == 1, TRUE, identical(x, y))

    rv <- r.from[i]
    ok3 <- rv %in% r.dict$Variable
    x <- sort(H[[2]]$levels)
    y <- sort(clean(filter(r.dict, Variable == rv)$Values))
    ok4 <- ifelse(length(x) == 1, TRUE, identical(x, y))

    # If both variables are present in the 'to' microdata but not consistent, print message suggesting they we re-harominzed
    if (ok1 & ok3 & (!ok2 | !ok4)) look <- c(look, i)

    # If everything looks OK, retain index of harmony to keep
    if (all(ok1, ok2, ok3, ok4)) keep <- c(keep, i)

  }

  #-----

  # Keep valid harmonies and write new .R harmony file to disk
  if (length(keep) > 0) {

    hfile <- hfile[keep]

    # Set the "modified" slots to current timestamp
    for (i in 1:length(hfile)) hfile[[i]]$modified <- as.character(Sys.time())

    # Write the harmony list to disk as .R file
    harmony2dotR(hfile, file.out = file.out)

    cat("Retained", length(keep), "valid harmonies.\n")

  } else {

    cat("No valid harmonies retained; nothing written to disk.\n")

  }

  #-----

  # Report which harmonies to take second look at
  if (length(look) > 0) {
    cat("The following harmonies were not strictly valid but the variables are present (look into it):\n")
    cat(paste(hnames[look], collapse = "\n"))
  }

  # # Report which harmonies had some
  # cat("The following harmonies were not retained:\n")
  # cat(paste(hnames[-c(keep, look)], collapse = "\n"))

}
