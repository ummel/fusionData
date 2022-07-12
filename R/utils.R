# Function to "clean" a numeric vector by reducing to significant digits and converting to integer, if possible
cleanNumeric <- function(x, convert = TRUE, ...) {
  x <- signifDigits(x, ...)
  if (convert) x <- convertInteger(x)
  return(x)
}

#------------------

# Function to return numeric vector rounded to reasonable significant digits
# Returns a significant digit-ized result that is within 'tol' (percent) of the original value for all observations
# If minimize = TRUE, function will try converting x to Z-scores first and 'tol' assesed relative to the Z-scores, then return result that minimizes number of unique values
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
    if (length(unique(x1)) <= length(unique(x2))) return(x1) else return(x2)
  }

}

#------------------

# Function to convert a numeric vector to integer, if possible
# Checks if maximum value is coercible to 32-bit integer; see ?integer "Details"
convertInteger <- function(x) {
  if (all(x[!is.na(x)] %% 1 == 0) & max(x, na.rm = TRUE) < 2*10^9) {
    return(as.integer(round(x)))
  } else {
    return(x)
  }
}

#-------------------

# Weighted quantile function
# Note that NA's are automatically removed
# n <- 1e3
# x <- sample(0:n, n)
# w <- sample(1:n, n)
# microbenchmark(median(rep(x, w)), unit = "us")
# microbenchmark(weightedQuantile(x, w), unit = "us")
# all(median(rep(x, w)) == weightedQuantile(x, w))

weightedQuantile <- function(x, w, p = 0.5) {

  if (missing(w)) w <- rep.int(1L, length(x))

  # Order the values and weights accordingly
  ord <- order(x, na.last = NA)  # This removes NA's from 'x'
  x <- x[ord]
  w <- w[ord]

  # Check that weights are positive
  w <- w / mean(w)  # To avoid possible integer overflow
  stopifnot(all(w > 0))

  # Extract the quantile values for each percentile in 'p'
  # This uses a stepfun to extract 'x' for the precise values of 'p'
  if (length(x) > 1) {
    out <- stepfun(x = (cumsum(w) / sum(w))[-length(x)], y = x)(p)
  } else {
    out <- x
  }

  return(out)

}

# Alternative using 'spatstat.geom', but looks slower (perhaps more precise)
# weightedQuantile <- function(x, w, probs = 0.5) {
#   cdf <- spatstat.geom::ewcdf(x = x, weights = w, normalise = TRUE)
#   spatstat.geom::quantile.ewcdf(x = cdf, names = FALSE, probs = probs)
# }

#-------------------

# Better variable abbreviation; used within summarizeSpatialDataset()
betterAbbreviate <- function(x) {
  y <- str_extract_all(x, "[a-zA-Z0-9]*")
  y <- str_squish(map_chr(y, paste, collapse = " "))
  y <- str_to_title(paste0(y, rep_len(letters, length(y))))
  abb <- tolower(abbreviate(y, named = FALSE))
  stopifnot(length(unique(abb)) == length(abb))
  return(abb)
}

#-------------------

# Function returns TRUE if 'x' has only one non-NA value
novary <- function(x) length(unique(na.omit(x))) == 1

#-------------------

# Function tries to make sure the returned factor is only ASCII-compliant characters
# This is mainly for future safety to prevent cross-platform of database issues with non-ASCII characters
safeCharacters <- function(x) {

  stopifnot(is.factor(x) | is.character(x))
  y <- y0 <- if (is.factor(x)) levels(x) else unique(as.character(x))

  # This code chunk attempts to ensure that the factor levels are all ASCII-compliant
  # If it detects non-ASCII strings, it attemps to convert using stringi::stri_trans_general()
  # However, this conversion may not work as-is on a Windows machine (see here: https://github.com/gagolews/stringi/issues/269)
  enc <- stringi::stri_enc_mark(y)
  fix <- which(!is.na(y) & enc != "ASCII")
  y[fix] <- stringi::stri_trans_general(y[fix], "Latin-ASCII")
  ascii <- stringi::stri_enc_mark(y) == "ASCII"  # Attempt to confirm that characters are all ASCII
  if (any(!ascii, na.rm = TRUE)) stop("Couldn't fix non-ASCII character(s) for strings:\n", paste(na.omit(unique(y[!ascii])), collapse = "\n"))

  # General text fix-ups for obvious/common errors
  y <- gsub('"', "", y, fixed = TRUE)  # Replace double-quote with blank
  y <- gsub(" ,", ",", y, fixed = TRUE)  # Remove space ahead of a comma
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

#------------------

# # Function to return weighted percentiles of 'x'; used by harmonize()
# # Percentiles are returned only if number of unique 'x' is at least 'min.unique'
# # This leaves variables like age or household size unaffected (original 'x' returned)
# # If the proportion of zero values is >= min.zero, then zeros are preserved in output
# # If zeros are preserved, then negative values are assigned percentiles ranging from 0 to -1, which the most negative value receiving -1
# # The logic here is that respondents tend to be accurate about -/0/+ classification of the response, but we want percentiles to capture the relative ranking within these classes
#
# convertPercentile <- function(x, w = NULL, min.unique = 100, min.zero = 0.05) {
#
#   i <- which(!is.na(x))
#
#   if (is.numeric(x) & length(unique(x[i])) >= min.unique) {
#
#     if (is.null(w)) w <- rep(1, length(x))
#
#     zeros <- sum(x[i] == 0) / length(i) >= min.zero
#     k <- if (zeros) i[x[i] != 0] else i
#
#     q <- if (zeros & any(x[k] > 0)) k[x[k] > 0] else k
#     cdf <- spatstat.geom::ewcdf(x[q], w[q])
#     x[q] <- cdf(x[q])
#
#     # Negative values - only relevant if zeros = TRUE
#     if (zeros & any(x[k] < 0)) {
#       q <- k[x[k] < 0]
#       cdf <- spatstat.geom::ewcdf(-x[q], w[q])
#       x[q] <- -cdf(-x[q])
#     }
#
#     # Reduce precision of output
#     x <- cleanNumeric(x, tol = 0.001)
#
#   }
#
#   return(x)
#
# }

#-------------------

# Function to return a robust scaled measure of a numeric/continuus variable; used with assemble()
# Scaled values are returned only if number of unique 'x' is at least 'min.unique'
# This leaves variables like age or household size unaffected (original 'x' returned)
# Original zeros are preserved in the output, with all other values converted to a robust Z-score: (x - median(x)) / mad(x)
# A final adjustment ensures that the (weighted) median of the scaled output equals 1 (if the weighted median is non-zero)
# This effectively assumes that conceptually similar variables with different measurement scales are sampling the same median household and zero-response households but could have varying scaled otherwise

# Example
# cei <- read_fst("survey-processed/CEX/CEI/CEI_2015-2019_H_processed.fst", columns = c("weight", "fincbtxm", "mrtgip"))
# acs <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst", columns = c("weight", "hincp", "mortgage"))
# test1 <- convert2scaled(cei$mrtgip, cei$weight)
# test2 <- convert2scaled(acs$mortgage, acs$weight)

convert2scaled <- function(x, w, min.unique = 100) {
  if (is.numeric(x) & length(unique(x)) >= min.unique) {
    i <- x != 0
    x0 <- x[i]
    xmed <- weightedQuantile(x0, w[i], p = 0.5)
    xmad <- 1.4826 * weightedQuantile(abs(x0 - xmed), w[i], p = 0.5)
    x0 <- (x0 - xmed) / xmad
    x[i] <- x0 + (xmed / xmad)
    xmed <- weightedQuantile(x, w, p = 0.5)
    if (xmed != 0) x <- x / xmed
    x <- cleanNumeric(x, tol = 0.001)
  }
  return(x)
}

#-------------------

# Function returning summary string for numeric variable
numFormat <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  paste(
    c("Min:", "Median:", " Mean:", "Max:"),
    cleanNumeric(c(min(x, na.rm = TRUE), weightedQuantile(x, w, p = 0.5), weighted.mean(x, w, na.rm = TRUE), max(x, na.rm = TRUE))),
    collapse = ", ")
}

#-------------------

# Function returning summary string for factor variable
fctFormat <- function(x) {
  paste(paste0("[", levels(x), "]"), collapse = ", ")
}

#-------------------

# Function to add a valid "pid" (person ID) column to person-level microdata
# This ensures that 'pid' is 1:n() for each household AND that the reference person is pid = 1, as expected by harmonize()
# hid: Variable indicating the unique household identifiers
# refvaf: Variable indicatin each person's relationship to reference person; reference person label must be the FIRST level
addPID <- function(data, hid, refvar) {
  stopifnot(is.factor(data[[refvar]]))
  cat("Reference person level:", levels(data[[refvar]])[1], "\n")
  data %>%
    arrange(across(all_of(c(hid, refvar)))) %>%
    group_by(across(all_of(hid))) %>%
    mutate(pid = 1L:n()) %>%
    ungroup() %>%
    labelled::set_variable_labels(.labels = list(pid = "Person identifier within household"))
}

#-------------------

# Function to treat integer and numeric as equal when checking for identical classes in prepare()
sameClass <- function(x, y) {
  if (x[1] == "integer") x <- "numeric"
  if (y[1] == "integer") y <- "numeric"
  identical(x, y)
}

#-------------------

# # Function to automatically detect outliers and set to NA using Rosner's test
# setOutliersNA <- function(x, ignore.zeros = TRUE) {
#   X <- if (ignore.zeros) na_if(x, 0) else x
#   K <- sum(0.6745 * (X - median(X, na.rm = TRUE)) / mad(X, constant = 1, na.rm = TRUE) > 3.5, na.rm = TRUE)
#   K <- max(1, min(K, floor(sum(!is.na(X)) / 2)))
#   rosner <- suppressWarnings(EnvStats::rosnerTest(X, k = K)$all.stats)
#   outlier.index <- rosner$Obs.Num[rosner$Outlier]
#   x[outlier.index] <- NA
#   return(x)
# }
