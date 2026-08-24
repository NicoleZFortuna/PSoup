# A function to comparatively plot the outcomes of different simulations

This function allows you to quickly plot the outcome of different
simulations.

## Usage

``` r
# S3 method for class 'simData'
plot(data, node, bioData = NA, ...)
```

## Arguments

- data:

  a data.frame containing the outcome of a set of simulations. The first
  row should contain the wild type condition.

- node:

  a string showing which node data will be plotted.

- bioData:

  a matrix containing the biological data corresponding to the simulated
  data.
