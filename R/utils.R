# Helper utility functions for internal package use within `fusionData`.
# These unexported utilities focus on data type conversion, numeric precision,
# spatial/survey variable transformations, and basic data integrity checks.


# Reduced Precision & Integer Coercion ------------------------------------

#' Reduce numeric precision and coerce to integer if possible
#'
#' Used across data harmonization steps to minimize memory overhead and storage size.
#' First attempts a direct integer conversion. If non-integers are present, it rounds
#' values to significant digits within a specified relative tolerance (`tol`) and
#' re-attempts integer coercion if the proportion of integer-like values exceeds `threshold`.
#'
#' @param x Numeric vector.
#' @param tol Relative numerical tolerance passed to `signifDigits()`. Default is 0.001.
#' @param minimize Logical. If `TRUE`, attempts Z-score transformation before rounding
#'   to see if it yields fewer unique values.
#' @param threshold Numeric fraction (0 to 1). Proportion of values that must be integer-like
#'   to trigger integer conversion.
#'
#' @return Numeric vector (coerced to integer class if criteria are met).
#'
#' @noRd
cleanNumeric <- function(x, tol = 0.001, minimize = FALSE, threshold = 0.999) {
  x <- convertInteger(x, threshold = 1)  # Coerces immediately to integer if entirely integer-like
  if (is.double(x)) {
    x <- signifDigits(x, tol = tol, minimize = minimize)
    x <- convertInteger(x, threshold = threshold)
  }
  return(x)
}


#' Round numeric vector to significant digits based on relative tolerance
#'
#' Ensures all transformed observations stay within `tol` (as a relative percentage)
#' of their original values. Useful for lossy numeric compression before storing microdata.
#'
#' @param x Numeric vector.
#' @param tol Relative error tolerance threshold (e.g., 0.001 = 0.1% max relative change).
#' @param minimize Logical. If `TRUE`, compares direct rounding against Z-score rounded
#'   values and returns whichever representation yields fewer unique values.
#'
#' @return Numeric vector rounded to significant digits.
#'
#' @noRd
signifDigits <- function(x, tol = 0.001, minimize = FALSE) {

  intFUN <- function(x, orig = x) {
    out <- rep(NA, length(x))
    out[x == 0 | is.na(x) | is.infinite(x)] <- 0
    i <- 1
    while (any(is.na(out))) {
      ind <- which(is.na(out))
      y <- x[ind]
      z <- abs(signif(y, i) - y) / abs(y)
      ok <- ind[z <= tol]
      out[ok] <- i
      i <- i + 1
    }
    return(signif(orig, out))
  }

  x1 <- intFUN(x)

  if (!minimize) {
    return(x1)
  } else {
    x2 <- intFUN(scale(x), x)
    if (data.table::uniqueN(x1) <= data.table::uniqueN(x2)) return(x1) else return(x2)
  }

}


#' Coerce numeric vector to integer class if safe
#'
#' Checks whether values are within 32-bit integer limits (`.Machine$integer.max`)
#' and whether the proportion of whole numbers meets or exceeds `threshold`.
#' Converts `allNA` vectors to logical.
#'
#' @param x Numeric vector.
#' @param threshold Numeric fraction (0 to 1). Minimum proportion of whole numbers required
#'   to convert entire vector to integer. Default is 0.99.
#'
#' @return Vector converted to integer, logical (if all NA), or unchanged numeric.
#'
#' @noRd
convertInteger <- function(x, threshold = 0.99) {
  if (collapse::allNA(x)) {
    x <- as.logical(x)
  } else {
    ok32 <- max(x, na.rm = TRUE) <= .Machine$integer.max
    if (ok32) {
      chk <- x[!is.na(x)] %% 1 == 0
      if (sum(chk) / length(chk) >= threshold) {
        x <- as.integer(round(x))
      }
    }
  }
  return(x)
}


# Weighted Calculations & Scaling ---------------------------------------

#' Weighted quantile estimation using step function empirical CDF
#'
#' Fast calculation of weighted sample quantiles (default is median).
#' Automatically removes NA values. Useful for survey data with observation weights (e.g., ACS).
#'
#' @param x Numeric vector of values.
#' @param w Numeric vector of sample weights. Defaults to unweighted (all 1s).
#' @param p Numeric vector of probabilities in `[0, 1]`. Default is 0.5 (median).
#'
#' @return Vector of estimated quantiles corresponding to `p`.
#'
#' @noRd
weightedQuantile <- function(x, w, p = 0.5) {

  if (missing(w)) w <- rep.int(1L, length(x))

  # Order the values and weights accordingly
  ord <- order(x, na.last = NA)  # Removes NA's from 'x'
  x <- x[ord]
  w <- w[ord]

  # Normalize weights to avoid potential integer overflow
  w <- w / mean(w)
  stopifnot(all(w > 0))

  # Extract quantile values using stepfun over the cumulative weight distribution
  if (length(x) > 1) {
    out <- stepfun(x = (cumsum(w) / sum(w))[-length(x)], y = x)(p)
  } else {
    out <- x
  }

  return(out)

}


#' Robust weighted scaling for continuous survey variables
#'
#' Used in `assemble()` to normalize continuous variables (like household income or mortgage payments)
#' across different survey datasets. Calculates robust Z-scores using median and MAD:
#' `(x - median) / mad`. Preserves original zeros and scales non-zero medians to 1.
#' Only transforms variables with at least `min.unique` unique non-zero values.
#'
#' @param x Numeric vector.
#' @param w Numeric vector of survey weights.
#' @param min.unique Minimum number of unique values required to perform scaling. Default is 100.
#' @param precision Number of significant digits for output rounding. Default is 3.
#'
#' @return Scaled numeric vector, or original vector if `uniqueN(x) < min.unique`.
#'
#' @noRd
convert2scaled <- function(x, w, min.unique = 100, precision = 3) {
  if (is.numeric(x) & data.table::uniqueN(x) >= min.unique) {
    i <- x != 0
    x0 <- x[i]
    xmed <- matrixStats::weightedMedian(x0, w[i])
    xmad <- 1.4826 * matrixStats::weightedMedian(abs(x0 - xmed), w[i])
    x0 <- (x0 - xmed) / xmad
    x[i] <- x0 + (xmed / xmad)
    xmed <- matrixStats::weightedMedian(x[i], w[i])
    if (xmed != 0) x <- x / xmed
    x <- signif(round(x, precision), precision)
  }
  return(x)
}


#' Convert numeric continuous features to positive integers via robust scaling
#'
#' Used within `assemble()` to compress spatial predictor datasets before saving to disk.
#' Uses robust median/MAD centering and scaling, applies a precision multiplier,
#' and applies a horizontal shift so all output values are non-negative integers.
#'
#' @param x Data frame, matrix, or numeric vector of features.
#' @param y Optional second data frame, matrix, or vector to scale using `x`'s parameters.
#' @param precision Integer power of 10 applied before rounding to preserve decimal detail.
#'
#' @return Scaled integer data frame/vector, or a list `list(x = ..., y = ...)` if `y` is provided.
#'
#' @noRd
scale2integer <- function(x, y = NULL, precision = 2) {

  vec <- FALSE
  if (is.vector(x)) {
    vec <- TRUE
    x <- data.frame(V1 = x)
  }
  stopifnot(all(apply(x, 2, is.double)))
  xcenter <- apply(x, 2, median, na.rm = TRUE)
  xscale <- apply(x, 2, mad, na.rm = TRUE)
  xscale[xscale == 0] <- apply(x[, xscale == 0], 2, sd, na.rm = TRUE)
  x <- scale(x, center = xcenter, scale = xscale)
  x <- round(x, precision)
  x <- apply(x, 2, function(x) as.integer(x * (10 ^ precision)))
  x <- data.frame(x)
  shift <- -1L * apply(x, 2, min, na.rm = TRUE)

  if (!is.null(y)) {
    if (vec) {
      y <- data.frame(V1 = y)
    } else {
      stopifnot(identical(colnames(x), colnames(y)))
    }
    stopifnot(all(apply(y, 2, is.double)))
    y <- scale(y, center = xcenter, scale = xscale)
    y <- round(y, precision)
    y <- apply(y, 2, function(x) as.integer(x * (10 ^ precision)))
    y <- data.frame(y)
    shift <- pmax(shift, -1L * apply(y, 2, min, na.rm = TRUE))
    y <- y + rep(shift, each = nrow(y))
    if (vec) y <- as.integer(y[[1]])
  }

  x <- x + rep(shift, each = nrow(x))
  if (vec) x <- as.integer(x[[1]])

  if (is.null(y)) {
    return(x)
  } else {
    return(list(x = x, y = y))
  }

}


#' Convert positive real weights to approximate integer weights
#'
#' Iteratively scales real-valued weights to integer values until the Pearson
#' correlation between original and integer weights meets `mincor`.
#'
#' @param x Numeric vector of strictly positive weights.
#' @param mincor Minimum acceptable Pearson correlation between original and integerized weights.
#'
#' @return Integer vector of converted weights.
#'
#' @noRd
integerize <- function(x, mincor = 0.999) {
  stopifnot(all(x > 0))
  if (sd(x) == 0) {
    return(rep(1L, length(x)))
  } else {
    p <- 0
    i <- 0
    r <- max(x) / min(x)
    while (p < mincor) {
      i <- i + 1
      mx <- ifelse(is.integer(x), r, max(r, 10 ^ i))
      z <- 1 + mx * ((x - min(x)) / r)
      z <- as.integer(round(z))
      p <- cor(x, z)
    }
    return(z)
  }
}


# String & Categorical Cleaners ------------------------------------------

#' Generate unique, clean, abbreviated variable names
#'
#' Helper used in `summarizeSpatialDataset()` to simplify lengthy geographic or spatial variable names.
#' Extracts alphanumeric tokens, converts to Title Case, runs `abbreviate()`, converts to lowercase,
#' and enforces uniqueness using `make.unique()`.
#'
#' @param x Character vector of variable names.
#'
#' @return Character vector of standardized short unique identifiers.
#'
#' @noRd
betterAbbreviate <- function(x) {
  y <- str_extract_all(x, "[a-zA-Z0-9]+")
  y <- str_squish(purrr::map_chr(y, paste, collapse = " "))
  y <- str_to_title(y)
  abb <- abbreviate(y, named = FALSE) |>
    tolower() |>
    make.unique(sep = "")
  stopifnot(length(unique(abb)) == length(abb))
  return(abb)
}


#' Sanitize strings and factor levels to ASCII encoding
#'
#' Cleans string or factor variables to prevent cross-platform encoding errors when writing to disk
#' or transferring across databases. Transliterates non-ASCII characters to Latin-ASCII,
#' strips extraneous double quotes, and removes leading/trailing spaces.
#'
#' @param x Character vector or factor.
#'
#' @return Character vector or factor matching input class, cleaned to ASCII standards.
#'
#' @noRd
safeCharacters <- function(x) {

  stopifnot(is.factor(x) | is.character(x))
  y <- y0 <- if (is.factor(x)) levels(x) else unique(as.character(x))

  # Transliterate non-ASCII strings using stringi
  enc <- stringi::stri_enc_mark(y)
  fix <- which(!is.na(y) & enc != "ASCII")
  y[fix] <- stringi::stri_trans_general(y[fix], "Latin-ASCII")
  ascii <- stringi::stri_enc_mark(y) == "ASCII"
  if (any(!ascii, na.rm = TRUE)) {
    stop("Couldn't fix non-ASCII character(s) for strings:\n", paste(na.omit(unique(y[!ascii])), collapse = "\n"))
  }

  # Text cleanup
  y <- gsub('"', "", y, fixed = TRUE)
  y <- gsub(" ,", ",", y, fixed = TRUE)
  y <- str_squish(y)

  if (identical(y, y0)) {
    x
  } else {
    if (is.factor(x)) {
      factor(y[as.integer(x)], levels = y, ordered = is.ordered(x))
    } else {
      y[match(x, y0)]
    }
  }

}


# Household Data Utilities ------------------------------------------------

#' Add sequential person ID within household
#'
#' Constructs a person identifier (`pid`) running from `1:n()` for each household unit.
#' Sorts member records such that the household reference person (first factor level in `refvar`)
#' is always assigned `pid = 1`, satisfying downstream requirements in `harmonize()`.
#'
#' @param data Data frame containing household microdata.
#' @param hid Column name identifying unique household IDs.
#' @param refvar Column name (factor) indicating relationship to reference person.
#'   The primary reference person label must be the FIRST factor level.
#'
#' @return Data frame with sorted rows and a newly appended, labelled `pid` column.
#'
#' @noRd
addPID <- function(data, hid, refvar) {
  stopifnot(is.factor(data[[refvar]]))
  cli::cli_inform("Reference person level: {.val {levels(data[[refvar]])[1]}}")
  data %>%
    arrange(across(all_of(c(hid, refvar)))) %>%
    group_by(across(all_of(hid))) %>%
    mutate(pid = 1L:n()) %>%
    ungroup() %>%
    labelled::set_variable_labels(.labels = list(pid = "Person identifier within household"))
}


# Summary Formatting & Inspection ----------------------------------------

#' Format summary string for numeric variables
#'
#' Generates a single-line text summary containing Min, Median, Mean, and Max,
#' applying `cleanNumeric()` for readable digit display. Supports observation weights.
#'
#' @param x Numeric vector.
#' @param w Optional numeric vector of sample weights.
#'
#' @return Single character string summarizing key central tendencies and range.
#'
#' @noRd
numFormat <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  paste(
    c("Min:", "Median:", " Mean:", "Max:"),
    cleanNumeric(c(min(x, na.rm = TRUE), matrixStats::weightedMedian(x, w), weighted.mean(x, w, na.rm = TRUE), max(x, na.rm = TRUE))),
    collapse = ", ")
}


#' Format summary string for categorical variables
#'
#' Displays available factor levels or logical levels as formatted bracketed strings
#' for logging and exploratory inspection.
#'
#' @param x Character vector, factor, or logical vector.
#'
#' @return Single character string listing unique levels/categories.
#'
#' @noRd
catFormat <- function(x) {
  stopifnot(!is.numeric(x))
  if (is.character(x)) x <- factor(x)
  if (is.logical(x)) "[TRUE], [FALSE]" else paste(paste0("[", levels(x), "]"), collapse = ", ")
}


# Data Structure & Logical Checks -----------------------------------------

#' Check if vector contains no variance or is entirely NA
#'
#' Returns `TRUE` if `x` contains 1 or 0 unique non-NA values.
#'
#' @param x Atomic vector or factor.
#'
#' @return Logical scalar (`TRUE` or `FALSE`).
#'
#' @noRd
novary <- function(x) data.table::uniqueN(x, na.rm = TRUE) <= 1


#' Compare data classes treating integer and numeric as equivalent
#'
#' Helper used during feature matching in `prepare()` to prevent class mismatch warnings
#' when comparing standard double numeric vectors against integer vectors.
#'
#' @param x Character vector representing standard R class name(s) (e.g., `class(vec1)`).
#' @param y Character vector representing standard R class name(s) (e.g., `class(vec2)`).
#'
#' @return Logical scalar indicating class equality.
#'
#' @noRd
sameClass <- function(x, y) {
  if (x[1] == "integer") x <- "numeric"
  if (y[1] == "integer") y <- "numeric"
  identical(x, y)
}


#' Compare vectors for value equality while ignoring factor levels and names
#'
#' Evaluates whether `x` and `y` have identical element values. Strips vector names
#' and converts factors to character vectors before evaluation.
#'
#' @param x Atomic vector or factor.
#' @param y Atomic vector or factor.
#'
#' @return Logical scalar (`TRUE` or `FALSE`).
#'
#' @noRd
identical2 <- function(x, y) {

  normalize <- function(v) {
    if (is.factor(v)) {
      return(as.character(v))
    }
    if (is.atomic(v)) {
      return(unname(v))
    }
    unname(v)
  }

  isTRUE(all.equal(normalize(x), normalize(y)))
}


#' Detect potential zero-inflation in numeric vector
#'
#' Checks whether a numeric vector contains at least 1% zeros and evaluates whether
#' the zero-mass relative density exceeds an arbitrary empirical threshold compared
#' to non-zero values.
#'
#' @param x Numeric vector.
#' @param threshold Ratio threshold evaluated at zero density. Default is 0.9.
#'
#' @return Logical scalar (`TRUE` if zero-inflated, `FALSE` otherwise).
#'
#' @noRd
inflated <- function(x, threshold = 0.9) {
  if (is.numeric(x)) {
    if (sum(x == 0) >= 0.01 * length(x)) {
      d1 <- density(x)
      d2 <- density(x[x != 0], bw = d1$bw, from = min(d1$x), to = max(d1$x))
      z <- which.min(abs(d1$x))
      d2$y[z] / d1$y[z] < threshold
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}


# System & Path Utilities ------------------------------------------------

#' Evaluate function call with default argument values included
#'
#' Wraps `match.call()` to explicitly include default formal argument values
#' from the calling function definition, enabling complete call inspection.
#'
#' @param ... Arguments passed to parent frame match call.
#' @param exclude Character vector of parameter names to omit from returned call object.
#'
#' @return Language object representing the expanded function call.
#'
#' @noRd
match.call.defaults <- function(..., exclude = NULL) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  for (i in setdiff(names(formals), c(names(call), exclude)))
    call[i] <- list(formals[[i]])
  match.call(sys.function(sys.parent()), call)
}


#' Return normalized file path using platform-specific separator
#'
#' Convenience wrapper around `normalizePath()` setting `winslash`
#' to current platform separator (`.Platform$file.sep`).
#'
#' @param path Character vector of file paths.
#' @param mustWork Logical passed to `normalizePath()`. Default is `NA`.
#'
#' @return Character vector of normalized file paths.
#'
#' @noRd
full.path <- function(path, mustWork = NA) {
  normalizePath(path = path, winslash = .Platform$file.sep, mustWork = mustWork)
}


#' Drop-in conditional statement preserving factor levels
#'
#' Modified wrapper around `dplyr::if_else()` that maintains original factor levels
#' and ordering of `yes` or `no` arguments in the resulting output.
#'
#' @param test Logical vector condition.
#' @param yes Value/vector to return when test is TRUE.
#' @param no Value/vector to return when test is FALSE.
#'
#' @return Vector matching structure of `yes`/`no` with factor levels preserved where applicable.
#'
#' @noRd
if.else <- function(test, yes, no) {
  out <- dplyr::if_else(test, yes, no)
  yl <- levels(yes)
  nl <- levels(no)
  if (length(yl) | length(nl)) {
    ou <- na.omit(unique(out))
    if (all(ou %in% nl)) out <- factor(out, levels = intersect(nl, ou), ordered = is.ordered(no))
    if (all(ou %in% yl)) out <- factor(out, levels = intersect(yl, ou), ordered = is.ordered(yes))
  }
  return(out)
}


#' Fast contingency and weighted frequency tables via data.table
#'
#' High-performance replacement for base `table()` that accepts observation weights
#' and efficiently handles NA grouping using `data.table`.
#'
#' @param x Atomic vector.
#' @param w Optional numeric vector of sample weights.
#' @param na.rm Logical. If `TRUE`, removes NA values from the frequency table. Default is `FALSE`.
#'
#' @return Named numeric vector of counts or weight sums, named by unique values of `x`.
#'
#' @noRd
table2 <- function(x, w = NULL, na.rm = FALSE) {
  require(data.table)
  stopifnot(is.atomic(x))
  if (is.null(w)) {
    ds <- setDT(list(x = x), key = "x")
    ds <- ds[, .N, by = "x"]
  } else {
    stopifnot(is.numeric(w) & length(w) == length(x))
    ds <- setDT(list(x = x, w = w), key = "x")
    ds <- ds[, .(N = sum(w)), by = "x"]
  }
  if (na.rm) ds <- na.omit(ds)
  return(setNames(ds$N, ds$x))
}


# Geographic Boundary Mappers -------------------------------------------

#' Map Connecticut Planning Region codes to legacy County FIPS codes
#'
#' Handles mixed-vintage Census datasets for Connecticut following the 2022 Census OMB adoption
#' of Planning Regions (110–190) as county-equivalents. Converts planning region codes back to
#' legacy 3-digit county FIPS (001–015) when present. Returns input unchanged if legacy codes are detected.
#'
#' @param x Character or numeric vector containing Connecticut county/planning region codes.
#'
#' @return Character vector formatted as 3-digit zero-padded county FIPS codes.
#'
#' @noRd
ct_planning_to_county_fips <- function(x) {

  x_chr <- as.character(x)

  # Detect whether planning-region codes (110-190) are present
  needs_conversion <- any(as.numeric(x_chr) >= 110 & as.numeric(x_chr) <= 190, na.rm = TRUE)

  if (!needs_conversion) return(sprintf("%03d", as.numeric(x_chr)))

  # Lookup map: Planning Region -> Legacy County FIPS
  lookup <- c(
    "110" = "003", # Capitol -> Hartford County
    "120" = "001", # Greater Bridgeport -> Fairfield County
    "190" = "001", # Western CT -> Fairfield County
    "130" = "007", # Lower CT River Valley -> Middlesex County
    "140" = "009", # Naugatuck Valley -> New Haven County
    "170" = "009", # South Central CT -> New Haven County
    "150" = "013", # Northeastern CT -> Tolland County
    "160" = "005", # Northwest Hills -> Litchfield County
    "180" = "011"  # Southeastern CT -> New London County
  )

  out <- x_chr

  idx <- x_chr %in% names(lookup)
  out[idx] <- lookup[x_chr[idx]]

  suppressWarnings({
    numeric_idx <- !idx
    out[numeric_idx] <- sprintf("%03d", as.numeric(out[numeric_idx]))
  })

  return(out)
}

#----------

# Saves R objects as .rda files to the local ./data directory.
# Replaces usethis::use_data() without requiring an active usethis project.

use_data2 <- function(..., overwrite = TRUE) {
  obj_names <- as.character(match.call(expand.dots = FALSE)$...)

  for (obj_name in obj_names) {
    file_path <- file.path("data", paste0(obj_name, ".rda"))

    if (file.exists(file_path) && !overwrite) {
      cli::cli_abort("File {.path {file_path}} already exists. Use {.code overwrite = TRUE} to overwrite.")
    }

    save(list = obj_name, file = file_path, envir = parent.frame())
    cli::cli_alert_success("Saved {.var {obj_name}} to {.path {file_path}}")
  }
}

#------------------

#' Query System Free Physical Memory
#'
#' @description
#' Queries OS system utilities across Windows, macOS, Linux, and HPC SLURM environments to estimate
#' available system RAM.
#'
#' @return Numeric scalar estimating available memory in megabytes (MB).
#'
#' @keywords internal
#' @noRd
freeMemory <- function() {
  gc()
  sys <- Sys.info()["sysname"]
  # Windows
  if (sys == "Windows") {
    x <- system2("wmic", args = "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.numeric(x) / 1e3
  } else {
    # Mac OS
    if (sys == "Darwin") {
      x <- system("vm_stat", intern = TRUE)
      pagesize <- x[grepl("Mach Virtual Memory Statistics",x)]
      pagesize <- gsub("Mach Virtual Memory Statistics: (page size of", "", pagesize, fixed = TRUE)
      pagesize <- gsub("bytes)", "", pagesize, fixed = TRUE)
      x <- x[grepl("Pages free: ", x)]
      x <- gsub("Pages free: ", "", x, fixed = TRUE)
      x <- gsub(".", "", x, fixed = TRUE)
      as.numeric(x) * as.numeric(pagesize) / (1024 ^ 2)
    } else {
      ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
      if (is.na(ncores)) {
        # Linux system assumed as backstop
        x <- system('grep MemAvailable /proc/meminfo', intern = TRUE)
        x <- strsplit(x, "\\s+")[[1]][2]
        as.numeric(x) / 1024
      } else {
        # Yale HPC setting
        ncores * as.integer(Sys.getenv("SLURM_MEM_PER_CPU"))
      }
    }
  }
}

