# A function to build a hormone object

Organises information into the correct format to describe a hormone

## Usage

``` r
buildHormone(nodeInfo, arcInfo, i, logicIndex, ids, nodesList, lang)
```

## Arguments

- nodeInfo:

  a data.frame containing summary information for nodes

- arcInfo:

  a data.frame containing arc information, including origins and
  destinations

- i:

  external index

- logicIndex:

  a vector consisting of the indexes of logical operators

- ids:

  a vector of all the ids

- nodesList:

  a list of all the nodes in the diagram

- lang:

  the SBGN language used to construct the original diagram
