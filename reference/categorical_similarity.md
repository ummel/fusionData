# Compute Total Variation Similarity Between Weighted Categorical Distributions

\`categorical_similarity()\` measures distributional similarity between
two weighted factor vectors using Total Variation (TV) distance across
shared factor levels.

## Usage

``` r
categorical_similarity(x, y, wx, wy)
```

## Arguments

- x:

  Factor vector. Categorical observations for the first group. Must
  share identical levels with `y`.

- y:

  Factor vector. Categorical observations for the second group. Must
  share identical levels with `x`.

- wx:

  Numeric vector. Sample weights for `x`.

- wy:

  Numeric vector. Sample weights for `y`.

## Value

A single numeric similarity score in \\\[0, 1\]\\, where 1 indicates
identical weighted category proportions.

## Details

Total Variation distance represents half the sum of absolute differences
between category proportions: \$\$\text{TV} = \frac{1}{2} \sum\_{k}
\|p_k - q_k\|\$\$ The resulting similarity score is \\1 - \text{TV}\\,
bounded in \\\[0, 1\]\\. A value of 1 indicates identical category
proportions, whereas 0 indicates completely disjoint distributions.

## See also

[`scaleNumeric`](https://ummel.github.io/fusionData/reference/scaleNumeric.md),
[`numerical_similarity`](https://ummel.github.io/fusionData/reference/numerical_similarity.md)
