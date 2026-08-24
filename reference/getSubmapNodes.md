# A function to pull out information from with in submaps

This function pulls out the nodes existing within a submap, as well as
collects the node information for processing.

## Usage

``` r
getSubmapNodes(node, comp, newNodes = NA)
```

## Arguments

- node:

  the parent node containing a submap

- comp:

  the compartment within which the submap sits

- newNodes:

  any carryover nodes in the case that the the function is being used
  recursively (if there are submaps within submaps)
