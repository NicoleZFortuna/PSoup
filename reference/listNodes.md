# A function to list all objects of class hormone in the current environment

A function to list all objects of class hormone in the current
environment

## Usage

``` r
listNodes(base = T)
```

## Arguments

- base:

  logical. Indicates whether the user wants to return a list of nodes
  that were contained in the original base package. Default is set to
  true. If the user wants to return nodes listed in the main environment
  (nodes that they have built themselves), they should set this
  parameter to FALSE.

## Examples

``` r
listNodes()
#> character(0)
```
