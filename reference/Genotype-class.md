# A function to generate objects of class 'Genotype'

A function to generate objects of class 'Genotype'

## Slots

- `name`:

  the name of the gene. Will be used as a tag to place influence within
  the network.

- `expression`:

  numeric vector with names "scion", and "rootstock". Numerical values
  set between 0 (no expression), and 1 (full expression). The default
  expression is 1 (the wildtype genotype).

- `coregulator`:

  of class character. The names of other genotypes that coregulate
  together.

- `influence`:

  a data.frame with the column names Node and Influence. Influences can
  one of either "production", "degradation", "inhibition", "perception".

## Examples

``` r
NA
#> [1] NA
```
