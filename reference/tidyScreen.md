# A function to make sure that data.frames containing conditions to screen are organised correctly. The wild-type condition is made to come first, and any duplicate rows are removed.

A function to make sure that data.frames containing conditions to screen
are organised correctly. The wild-type condition is made to come first,
and any duplicate rows are removed.

## Usage

``` r
tidyScreen(frame, name, exogenous = FALSE, preventDrop = F)
```

## Arguments

- frame:

  a data.frame

- name:

  a string giving the name of the data frame in case a warning is
  generated.

- exogenous:

  logical. Indicates if the wild type row should be 1s or 0s. if
  checking modifier or node screens, should be set to F. If checking
  exogenous screens, should be T.

- preventDrop:

  logical. Set to FALSE by default. If set to TRUE, prevents duplicated
  rows from being removed.
