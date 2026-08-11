#' Convey Existing Survey Variable Harmonies to New Survey Pairs
#'
#' @description
#' Transfers variable harmonization specifications from an existing harmony file
#' (`from`) to a new target survey pair (`to`). This function accelerates the
#' harmonization workflow when introducing a new vintage of an existing survey
#' by re-using previously defined, validated variable mappings.
#'
#' @param from Character. File name of an existing R harmony specification file
#'   located in `harmony/harmonies/` (e.g., `"RECS_2015__ACS_2019.R"`).
#' @param to Character. File name for the target harmony specification file to be
#'   created in `harmony/harmonies/` (e.g., `"RECS_2015__ACS_2015.R"`).
#' @param overwrite Logical. If `TRUE`, overwrites `to` if it already exists on disk.
#'   Defaults to `FALSE`.
#'
#' @details
#' A harmony mapping in `from` is considered **valid** and automatically conveyed
#' to `to` only if:
#' 1. Both donor and recipient variables exist in the dictionary metadata for the
#'    target survey pair.
#' 2. The expected factor level definitions in the source harmony match the target
#'    survey dictionary values exactly (for categorical variables).
#'
#' If variables exist in the target surveys but their factor level structures differ
#' (e.g., revised response categories in a newer survey vintage), `conveyHarmony()`
#' excludes the mapping from the new file and outputs a console message identifying
#' candidates for manual re-harmonization via the `harmony` Shiny application.
#'
#' @section Directory Requirement:
#' **Important:** This function must be executed with your R working directory set to
#' the root of the local `fusionData` repository (e.g., `setwd("path/to/fusionData")`).
#' It reads from `harmony/harmonies/` and sources `harmony/R/harmony2dotR.R`.
#'
#' @section Workflow Note:
#' While `conveyHarmony()` automates the transfer of identical mappings, **users
#' should always manually inspect and verify the resulting `.R` file in `harmony/harmonies/`**
#' or load it in the `harmony` Shiny application.
#'
#' @return Invisibly returns `NULL`. As a side effect, writes a new R script containing
#' the retained harmony list object to `harmony/harmonies/[to]` if at least one valid
#' harmony is identified.
#'
#' @seealso \code{\link[fusionData]{dictionary}}
#'
#' @examples
#' \dontrun{
#' # Ensure working directory is set to the fusionData repository root
#' conveyHarmony(
#'   from = "RECS_2015__ACS_2019.R",
#'   to = "RECS_2015__ACS_2015.R",
#'   overwrite = TRUE
#' )
#' }
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

  # Load helper utility required to serialize harmony list objects into R code
  source("harmony/R/harmony2dotR.R")

  # Split donor__recipient variable strings on double-underscore delimiter
  splitNames <- function(x) strsplit(x, "__", fixed = TRUE)

  # Extract bracketed categorical factor levels from dictionary 'Values' strings
  clean <- function(x) gsub("[", "", gsub("]", "", str_squish(unlist(strsplit(x, split = "], ", fixed = T))), fixed = T), fixed = T)

  # Load variable dictionary
  load("harmony/www/dictionary.rda")

  # Extract existing harmony specifications and parse donor/recipient variable names
  hfile <- dget(paste0("harmony/harmonies/", from))
  hnames <- names(hfile)
  d.from <- map_chr(splitNames(hnames), 1)
  r.from <- map_chr(splitNames(hnames), 2)

  # Validate output path and check overwrite permissions
  file.out <- paste0("harmony/harmonies/", to)
  if (!overwrite & file.exists(file.out)) stop("The 'to' harmony file already exists; must set 'overwrite = TRUE'")

  # Retrieve dictionary metadata for target donor and recipient survey vintages
  to.surveys <- gsub(".R", "", unlist(splitNames(to)), fixed = TRUE)
  d.dict <- filter(dictionary, paste(Survey, Vintage, sep = "_") %in% to.surveys[1])
  r.dict <- filter(dictionary, paste(Survey, Vintage, sep = "_") %in% to.surveys[2])

  # Initialize index tracking for retained and candidate harmonies
  keep <- NULL
  look <- NULL

  # Evaluate validity of each source harmony against target survey dictionaries
  for (i in 1:length(hfile)) {

    H <- hfile[[i]]

    # Check donor variable presence and factor level equivalence
    dv <- d.from[i]
    ok1 <- dv %in% d.dict$Variable
    x <- sort(H[[1]]$levels)
    y <- unique(sort(clean(filter(d.dict, Variable == dv)$Values)))
    ok2 <- ifelse(length(x) == 1, TRUE, identical2(x, y))

    # Check recipient variable presence and factor level equivalence
    rv <- r.from[i]
    ok3 <- rv %in% r.dict$Variable
    x <- sort(H[[2]]$levels)
    y <- unique(sort(clean(filter(r.dict, Variable == rv)$Values)))
    ok4 <- ifelse(length(x) == 1, TRUE, identical2(x, y))

    # Log index if variables are present in target surveys but factor level structures differ
    if (ok1 & ok3 & (!ok2 | !ok4)) look <- c(look, i)

    # Retain index if variable names and levels match target dictionaries perfectly
    if (all(ok1, ok2, ok3, ok4)) keep <- c(keep, i)

  }

  # Write valid transferred harmonies to disk
  if (length(keep) > 0) {

    hfile <- hfile[keep]

    # Update modification timestamp for transferred harmony rules
    for (i in 1:length(hfile)) hfile[[i]]$modified <- as.character(Sys.time())

    # Write serialized harmony list to target .R file
    harmony2dotR(hfile, file.out = file.out)

    cli::cli_inform(c("v" = "Retained {.val {length(keep)}} valid harmony mapping{?s}."))

  } else {

    cli::cli_inform(c("x" = "No valid harmonies retained; nothing written to disk."))

  }

  # Report present variables requiring manual factor level review in the Harmony app
  if (length(look) > 0) {
    cli::cli_inform(c(
      "!" = "The following {.val {length(look)}} harmony mapping{?s} were not strictly valid due to level mismatches, but variables exist in target surveys:",
      "*" = "{hnames[look]}"
    ))
  }

}
