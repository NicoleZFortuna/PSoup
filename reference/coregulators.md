# A function that creates a string with coregulators multiplied together

Coregulators are sorted so that their multiplications are only
represented once. Each modulator is given a temporal modifier. Sets of
coregulators are summed.

## Usage

``` r
coregulators(coreg, returnNum = FALSE, language, operator, sharp = FALSE)
```

## Arguments

- coreg:

  a subset of a the inputs of a hormone object. This subset must all
  have the same modulating effect, and have coregulators

- returnNum:

  return the number of unique coregulator sets. Default is set to FALSE.

- language:

  which programming language should the equation be generated in? Can be
  either "R", or "C".

- operator:

  which operator defines the coregulator, either "and" or "or".

- sharp:

  this argument is used in conjunction with the language argument. If
  language = "C", provide a logical value as to whether it is C# or not.
