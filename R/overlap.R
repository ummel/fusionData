#' Overlap coefficient between two Normal distributions
#'
#' @description
#' Based on example and code \href{https://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions}{provided here}.
#'
#' @param mu1 Mean of the first distribution.
#' @param mu2 Mean of the second distribution.
#' @param sd1 Standard deviation of the first distribution.
#' @param sd2 Standard deviation of the second distribution.
#' @param plot Logical. If `TRUE`, PDF's of the distributions are plotted with the overlap area highlighted.
#'
#' @details Calculates the overlap coefficient, an intuitive measure of similarity between two Normal distributions.
#'
#' @references \href{https://www.tandfonline.com/doi/abs/10.1080/03610928908830127}{Inman and Bradley (1989)}
#'
#' @return Numeric value `[0, 1]` indicating extent of overlap.
#'
#' @examples
#' overlap(mu1 = 10, mu2 = 14, sd1 = 2, sd2 = 3, plot = TRUE)
#'
#' @export

overlap <- function(mu1, mu2, sd1, sd2, plot = FALSE) {

  min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
    f1 <- dnorm(x, mean=mu1, sd=sd1)
    f2 <- dnorm(x, mean=mu2, sd=sd2)
    pmin(f1, f2)
  }

  xs <- seq(min(mu1 - 5*sd1, mu2 - 5*sd2), max(mu1 + 5*sd1, mu2 + 5*sd2), length.out = 500)
  f1 <- dnorm(xs, mean=mu1, sd=sd1)
  f2 <- dnorm(xs, mean=mu2, sd=sd2)

  if (plot) {
    plot(xs, f1, type="l", ylim=c(0, max(f1,f2)), ylab="density", col = 2, lwd = 2)
    lines(xs, f2, col = 4, lwd = 2)
    ys <- min.f1f2(xs, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
    xs <- c(xs, xs[1])
    ys <- c(ys, ys[1])
    polygon(xs, ys, col = "lightgray", border = NA)
  }

  # Scale inputs by 'mu1' for numeric stability prior to integration
  mu2 <- mu2 / mu1
  sd1 <- sd1 / mu1
  sd2 <- sd2 / mu1
  mu1 <- 1

  # Integrate to find overlap coefficient
  out <- integrate(min.f1f2, lower = -Inf, upper = Inf, mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2)
  out$value

}
