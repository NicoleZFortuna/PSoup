# A function to generate an equation for a specific node

A function to generate an equation for a specific node

## Usage

``` r
generateEquation(
  node,
  genotypes,
  language,
  ruleStyle = "Dun",
  necStimFunc = NULL,
  threshold = NULL,
  sharp = FALSE,
  exogenous = TRUE
)
```

## Arguments

- node:

  a node from within a network object

- genotypes:

  the list of genes frome within a network object

- language:

  which programming language should the equation be generated in? Can be
  either "R", or "C".

- ruleStyle:

  either "Dun", or "Mike". The Dun style resembles the original Dun
  equations normalised such that WT conditions are always 1. The Mike
  style creates mirrored stimulatory and inhibitory effects.

- necStimFunc:

  a data.frame containing the columns: necInput, and function. The name
  of the function will be applied to the indicated necessary stimulant.
  The default is NULL, in which case no function will be applied to
  necessary stimulants, and therefore the form will be linear.

- threshold:

  if the necStimFunc has a threshold, the argument name for that
  threshold should be specified here.

- sharp:

  this argument is used in conjunction with the language argument. If
  language = "C", provide a logical value as to whether it is C# or not.

- exogenous:

  logical. Specifies if the equation should include an exogenous supply
  term.
