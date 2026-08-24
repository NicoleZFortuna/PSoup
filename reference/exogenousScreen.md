# A function to generate all combinations of two vectors containing the values to be screened for two nodes. This function will also include the possibility of no exogenous values being provided in the form of NA.

A function to generate all combinations of two vectors containing the
values to be screened for two nodes. This function will also include the
possibility of no exogenous values being provided in the form of NA.

## Usage

``` r
exogenousScreen(nodes, screen1, screen2, folder)
```

## Arguments

- nodes:

  a vector containing the names of two nodes of interest

- screen1:

  a vector containing all the values to be tested for the first listed
  node.

- screen2:

  a vector containing all the values to be tested for the second listed
  node.

- folder:

  the directory for the model folder in which to save the output of this
  function.
