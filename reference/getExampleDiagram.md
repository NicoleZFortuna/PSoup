# a function to move the example .sbgn file into a folder of the users choice.

a function to move the example .sbgn file into a folder of the users
choice.

## Usage

``` r
getExampleDiagram(folder, ...)
```

## Arguments

- folder:

  a folder on the users computer. If the folder exists already on the
  users computer, the file will be moved there, otherwise the folder
  will be generated before moving the file to the specified location.

## Details

The function will return TRUE if the file has been placed in the
requested folder. If the file already exists in the specified location,
FALSE will be returned.
