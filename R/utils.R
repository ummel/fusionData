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

#------------------

# Weighted empirical cumulative distribution function
# Code take from spatstat.geom::ewcdf()
# See here: https://github.com/spatstat/spatstat.geom/blob/main/R/ewcdf.R
#wecdf <- spatstat.geom::ewcdf

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
  y <- as.character(x)

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

  if (is.factor(x)) {
    factor(y, levels = y[match(levels(x), x)], ordered = is.ordered(x))
  } else {
    y
  }

}

#-------------------

# Calculate weighted percentiles of 'x', retaining original zeros
# convertPercentile <- function(x, w) {
#   if (missing(w)) w <- rep.int(1, length(x))
#   stopifnot(length(x) == length(w))
#   z <- x != 0
#   y <- x[z]
#   n <- length(unique(y))  # Number of unique, non-zero values
#   if (n > 50) {  # Arbitrary minimum to determine when percentiles are appropriate
#     cdf <- wecdf(y, weights = w)
#     x[z] <- cdf(y)  # Replace non-zero values with weighted percentile
#     return(x)
#   } else {
#     return(x)
#   }
# }

