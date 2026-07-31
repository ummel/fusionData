# Scale and Harmonize Weighted Numeric Vectors

\`scaleNumeric()\` pre-processes two weighted numeric vectors prior to
evaluating their distribution similarity. It applies adaptive scaling
based on distribution cardinality and zero-inflation, returning both the
scaled vectors and their resulting similarity score.

## Usage

``` r
scaleNumeric(x1, x2, w1, w2)
```

## Arguments

- x1:

  Numeric vector. The first numeric variable (e.g., donor observation
  values).

- x2:

  Numeric vector. The second numeric variable (e.g., recipient
  observation values).

- w1:

  Numeric vector. Sample weights for `x1`.

- w2:

  Numeric vector. Sample weights for `x2`.

## Value

A `list` containing three elements:

- `[[1]]`:

  Transformed/scaled version of `x1`.

- `[[2]]`:

  Transformed/scaled version of `x2`.

- `[[3]]`:

  Numeric similarity score in \\\[0, 1\]\\ calculated via
  [`numerical_similarity`](https://ummel.github.io/fusionData/reference/numerical_similarity.md).

## Details

To make numerical comparisons robust across diverse variables,
\`scaleNumeric()\` uses two distinct transformation paths:

- **High-cardinality (\>100 unique non-NA values)**: Standardizes
  vectors using weighted medians and weighted Median Absolute Deviation
  (MAD). If the MAD is zero, it falls back to weighted standard
  deviation. Standardized values are rounded and scaled to integer
  Z-scores to save memory and optimize downstream comparisons. For
  zero-inflated distributions, the similarity score is evaluated both
  overall and on non-zero subsets, taking the maximum of the two.

- **Low-cardinality (\\\le 100\\ unique values)**: Converts unique
  values across both vectors to unified dense integer ranks using
  [`match`](https://rdrr.io/r/base/match.html).

## See also

[`numerical_similarity`](https://ummel.github.io/fusionData/reference/numerical_similarity.md),
[`categorical_similarity`](https://ummel.github.io/fusionData/reference/categorical_similarity.md),
[`fusionInput`](https://ummel.github.io/fusionData/reference/fusionInput.md)
