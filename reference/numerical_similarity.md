# Compute Quantile-Based Similarity Between Weighted Numeric Distributions

\`numerical_similarity()\` evaluates the distributional similarity
between two weighted numeric vectors by comparing their weighted
quantile functions across a fine grid.

## Usage

``` r
numerical_similarity(x, y, wx, wy)
```

## Arguments

- x:

  Numeric vector. Observations from the first distribution.

- y:

  Numeric vector. Observations from the second distribution.

- wx:

  Numeric vector. Sample weights for `x`.

- wy:

  Numeric vector. Sample weights for `y`.

## Value

A single numeric similarity score in \\\[0, 1\]\\, where 1 represents
identical weighted quantile distributions and 0 represents complete
dissimilarity.

## Details

The comparison uses a grid of 1,000 quantiles evaluated via
[`fquantile`](https://fastverse.org/collapse/reference/fquantile.html).
Quantile differences are weighted using a Gaussian kernel centered at
the median (\\p = 0.5\\, \\\sigma = 0.15\\) to emphasize central
distributional overlap while deemphasizing extreme tail behavior.

The score is calculated as a normalized relative distance subtracted
from 1: \$\$\text{Similarity} = 1 - \frac{\sum w_i \|q\_{x,i} -
q\_{y,i}\|}{\sum w_i \frac{\|q\_{x,i}\| + \|q\_{y,i}\|}{2}}\$\$

## See also

[`scaleNumeric`](https://ummel.github.io/fusionData/reference/scaleNumeric.md),
[`categorical_similarity`](https://ummel.github.io/fusionData/reference/categorical_similarity.md)
