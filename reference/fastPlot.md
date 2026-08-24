# A plot to quickly inspect the progression of a simulation.

This function is not designed to produce high quality plots. It is
simply to quickly inspect the progression of a simulation.

## Usage

``` r
fastPlot(sim, logTransform = T, removeBaseline = T)
```

## Arguments

- sim:

  the output of a single simulation.

- logTransform:

  defaults to TRUE. Indicates if the data should be log transformed.

- removeBaseline:

  defualts to TRUE Indicates if nodes that have remained at baseline
  throughout the simulation should be removed.
