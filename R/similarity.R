#' Scale and Harmonize Weighted Numeric Vectors
#'
#' @description
#' `scaleNumeric()` pre-processes two weighted numeric vectors prior to evaluating their
#' distribution similarity. It applies adaptive scaling based on distribution cardinality
#' and zero-inflation, returning both the scaled vectors and their resulting similarity score.
#'
#' @details
#' To make numerical comparisons robust across diverse variables, `scaleNumeric()` uses
#' two distinct transformation paths:
#' \itemize{
#'   \item \strong{High-cardinality (>100 unique non-NA values)}: Standardizes vectors using
#'   weighted medians and weighted Median Absolute Deviation (MAD). If the MAD is zero,
#'   it falls back to weighted standard deviation. Standardized values are rounded and
#'   scaled to integer Z-scores to save memory and optimize downstream comparisons.
#'   For zero-inflated distributions, the similarity score is evaluated both overall and
#'   on non-zero subsets, taking the maximum of the two.
#'   \item \strong{Low-cardinality (\eqn{\le 100} unique values)}: Converts unique values across
#'   both vectors to unified dense integer ranks using \code{\link[base]{match}}.
#' }
#'
#' @param x1 Numeric vector. The first numeric variable (e.g., donor observation values).
#' @param x2 Numeric vector. The second numeric variable (e.g., recipient observation values).
#' @param w1 Numeric vector. Sample weights for \code{x1}.
#' @param w2 Numeric vector. Sample weights for \code{x2}.
#'
#' @return A \code{list} containing three elements:
#'   \item{\code{[[1]]}}{Transformed/scaled version of \code{x1}.}
#'   \item{\code{[[2]]}}{Transformed/scaled version of \code{x2}.}
#'   \item{\code{[[3]]}}{Numeric similarity score in \eqn{[0, 1]} calculated via \code{\link{numerical_similarity}}.}
#'
#' @keywords internal
#' @seealso \code{\link{numerical_similarity}}, \code{\link{categorical_similarity}}, \code{\link{fusionInput}}
#' @noMd

scaleNumeric <- function(x1, x2, w1, w2) {
  stopifnot(!anyNA(x1) & !anyNA(x2))

  # For zero-inflated/sparse distributions, identify indices of non-zero elements
  if (inflated(x1)) {
    i <- which(x1 != 0)
    j <- which(x2 != 0)
  } else {
    i <- seq_along(x1)
    j <- seq_along(x2)
  }

  if (uniqueN(x1[i], na.rm = TRUE) > 100) {
    # Convert high-cardinality continuous variables to robust, integerized Z-scores
    # Standardizing by weighted median and MAD reduces sensitivity to extreme outliers
    med1 <- matrixStats::weightedMedian(x1[i], w1[i], na.rm = TRUE)
    med2 <- matrixStats::weightedMedian(x2[j], w2[j], na.rm = TRUE)
    mad1 <- matrixStats::weightedMad(x1[i], w1[i], na.rm = TRUE)
    mad2 <- matrixStats::weightedMad(x2[j], w2[j], na.rm = TRUE)

    # Fallback to weighted standard deviation if MAD is zero (e.g., highly concentrated distributions)
    if (mad1 == 0 | mad2 == 0) {
      mad1 <- matrixStats::weightedSd(x1[i], w1[i], na.rm = TRUE)
      mad2 <- matrixStats::weightedSd(x2[j], w2[j], na.rm = TRUE)
    }

    # Scale to integerized Z-scores (multiply by 1000 and cast to integer for efficient storage)
    x1 <- as.integer(round((x1 - med1) / mad1, 3) * 1e3)
    x2 <- as.integer(round((x2 - med2) / mad2, 3) * 1e3)

    # Compute similarity both with and without zero values, taking the higher score
    sim1 <- numerical_similarity(x1, x2, w1, w2)
    sim2 <- numerical_similarity(x1[i], x2[j], w1[i], w2[j])
    sim <- max(sim1, sim2)  # Return maximum of similarities calculated with and without zeros
  } else {
    # For low-cardinality variables, map unique values across both vectors to dense integer ranks
    sim <- numerical_similarity(x1, x2, w1, w2)
    u <- sort(unique(c(x1, x2)))
    d1 <- match(x1, u)
    d2 <- match(x2, u)
  }
  return(list(x1, x2, sim))
}

#--------------

#' Compute Quantile-Based Similarity Between Weighted Numeric Distributions
#'
#' @description
#' `numerical_similarity()` evaluates the distributional similarity between two weighted
#' numeric vectors by comparing their weighted quantile functions across a fine grid.
#'
#' @details
#' The comparison uses a grid of 1,000 quantiles evaluated via \code{\link[collapse]{fquantile}}.
#' Quantile differences are weighted using a Gaussian kernel centered at the median (\eqn{p = 0.5},
#' \eqn{\sigma = 0.15}) to emphasize central distributional overlap while deemphasizing extreme tail behavior.
#'
#' The score is calculated as a normalized relative distance subtracted from 1:
#' \deqn{\text{Similarity} = 1 - \frac{\sum w_i |q_{x,i} - q_{y,i}|}{\sum w_i \frac{|q_{x,i}| + |q_{y,i}|}{2}}}
#'
#' @param x Numeric vector. Observations from the first distribution.
#' @param y Numeric vector. Observations from the second distribution.
#' @param wx Numeric vector. Sample weights for \code{x}.
#' @param wy Numeric vector. Sample weights for \code{y}.
#'
#' @return A single numeric similarity score in \eqn{[0, 1]}, where 1 represents identical
#'   weighted quantile distributions and 0 represents complete dissimilarity.
#'
#' @keywords internal
#' @seealso \code{\link{scaleNumeric}}, \code{\link{categorical_similarity}}
#' @noMd

numerical_similarity <- function(x, y, wx, wy) {
  # Generate fine evaluation grid across probabilities [0, 1]
  p <- seq(0, 1, length.out = 1e3)

  # Estimate weighted quantile functions for both vectors
  qx <- collapse::fquantile(x, probs = p, w = wx)
  qy <- collapse::fquantile(y, probs = p, w = wy)

  # Construct Gaussian weighting function centered at p = 0.5 to downweight extreme tails
  w <- exp(-((p - 0.5)^2) / (2 * 0.15^2))
  w <- w / sum(w)

  # Calculate weighted relative L1 distance
  num <- weighted.mean(abs(qx - qy), w)
  den <- weighted.mean((abs(qx) + abs(qy)) / 2, w)
  sim <- if (den == 0) 1 else 1 - num / den
  return(sim)
}

#--------------

#' Compute Total Variation Similarity Between Weighted Categorical Distributions
#'
#' @description
#' `categorical_similarity()` measures distributional similarity between two weighted factor
#' vectors using Total Variation (TV) distance across shared factor levels.
#'
#' @details
#' Total Variation distance represents half the sum of absolute differences between category
#' proportions:
#' \deqn{\text{TV} = \frac{1}{2} \sum_{k} |p_k - q_k|}
#' The resulting similarity score is \eqn{1 - \text{TV}}, bounded in \eqn{[0, 1]}. A value of 1
#' indicates identical category proportions, whereas 0 indicates completely disjoint distributions.
#'
#' @param x Factor vector. Categorical observations for the first group. Must share identical
#'   levels with \code{y}.
#' @param y Factor vector. Categorical observations for the second group. Must share identical
#'   levels with \code{x}.
#' @param wx Numeric vector. Sample weights for \code{x}.
#' @param wy Numeric vector. Sample weights for \code{y}.
#'
#' @return A single numeric similarity score in \eqn{[0, 1]}, where 1 indicates identical
#'   weighted category proportions.
#'
#' @keywords internal
#' @seealso \code{\link{scaleNumeric}}, \code{\link{numerical_similarity}}
#' @noMd

categorical_similarity <- function(x, y, wx, wy) {
  stopifnot(is.factor(x), is.factor(y))
  stopifnot(identical(levels(x), levels(y)))

  # Compute weighted sum per category level using fast aggregation from 'collapse'
  p <- collapse::fsum(wx, x, use.g.names = FALSE)
  p <- p / sum(p)  # Normalize to category probabilities for x

  q <- collapse::fsum(wy, y, use.g.names = FALSE)
  q <- q / sum(q)  # Normalize to category probabilities for y

  # Calculate Total Variation distance and convert to similarity score
  tv <- sum(abs(p - q)) / 2
  1 - tv
}
