# A function to generate a screen of modifier conditions based on a set of prior distributions.

A function to generate a screen of modifier conditions based on a set of
prior distributions.

## Usage

``` r
modifierPriorScreen(
  folder,
  priorDistribution = "logNormal",
  n,
  returnVals = FALSE,
  minVal = 0,
  maxVal = 2,
  savePriors = T
)
```

## Arguments

- folder:

  a string stating the directory of the folder containing your generated
  model.

- priorDistribution:

  states the prior distribution to be used to generate modifier values.
  If of length 1, the prior will be applied to all modifier values. If
  length is greater than 1, the vector must be named with the
  corresponding modifier names. To specify a value for particular
  modifiers, provide the value of that modifier instead of the
  distribution to be used. Available distributions and be either
  'logNormal', or 'uniform'. Default is set to 'logNormal'. if 'uniform'
  is chosen, you must provide values for the minVal and maxVal
  arguments.

- n:

  the number of simulations for which a set of priors will be generated.

- returnVals:

  logical. If the output should be returned to the user.

- minVal:

  default set to 0. The minimum starting value for a node.

- maxVal:

  default set to 2. The maximum starting value for a node.
