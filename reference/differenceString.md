# A function to build the difference equation including delays

A function to build the difference equation including delays

## Usage

``` r
differenceString(string, delays = NA, takeProduct = FALSE, language)
```

## Arguments

- string:

  whatever string is being constructed.

- delays:

  a vector specifying if there are any delays associated with a
  particular input.

- takeProduct:

  logical. Should be set to TRUE if this function is being used to
  collapse necessary stimulants (\* instead of +). Can be set to NULL of
  you do not want to concatenate into a single string.

- language:

  which programming language should the equation be generated in? Can be
  either "R", or "C".
