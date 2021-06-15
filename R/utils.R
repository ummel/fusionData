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
convertInteger <- function(x) {
  if (all(x[!is.na(x)] %% 1 == 0)) {
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

# Calculate weighted percentiles of 'x', retaining original zeros
convertPercentile <- function(x, w) {
  if (missing(w)) w <- rep.int(1, length(x))
  stopifnot(length(x) == length(w))
  z <- x != 0
  y <- x[z]
  n <- length(unique(y))  # Number of unique, non-zero values
  if (n > 50) {  # Arbitrary minimum to determine when percentiles are appropriate
    cdf <- wecdf(y, weights = w)
    x[z] <- cdf(y)  # Replace non-zero values with weighted percentile
    return(x)
  } else {
    return(x)
  }
}
