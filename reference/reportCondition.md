# A function to report any deviation from the baseline for simulations

A function to report any deviation from the baseline for simulations

## Usage

``` r
reportCondition(sims)
```

## Arguments

- sims:

  either the full output list returned by setupSims, in which case the
  deviations of the full simulations will be returned. Otherwise, if
  only a single simulation is provided (in the form
  output\$screen\[\[index\]\]), the deviation of that specific
  simulation condition will be returned.
