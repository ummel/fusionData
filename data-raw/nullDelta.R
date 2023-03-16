# Generate a function that returns expected "null" delta metric for comparison of randomly generated proportions
# This is used to calculate a similarity scores comparing donor and recipient proportions for harmonized variables

# Expected absolute error/delta if proportions drawn randomly
# The expected error increases with 'n' but appears to have asymptotic value of 2/3
nseq <- c(1:10, 20, 30, 50, 100)
null.delta <- sapply(nseq, function(n) {
  out <- replicate(10e3, {
    x <- runif(n); x <- x / sum(x)
    y <- runif(n); y <- y / sum(y)
    sum(abs(x - y))
  })
  mean(out)
}) %>%
  pmin(2 / 3) %>%
  sort()

# Fit a linear approximation function
nullDelta <- approxfun(x = nseq,
                       y = null.delta,
                       rule = 1:2)

# Save nullDelta() function to disk
usethis::use_data(nullDelta, overwrite = TRUE)
