# A function to generate a data frame which lists all of the edges of a network.

A function to generate a data frame which lists all of the edges of a
network.

## Usage

``` r
generateEdgeList(
  network,
  keepAltSource = F,
  keepNecessaryStimulant = F,
  recordConjunctions = F
)
```

## Arguments

- network:

  an object of class Network.

- keepAltSource:

  default set to FALSE. Specify if you want to recognise if the incoming
  node is an alternate source of the same 'hormone' type. If not, these
  alternative sources will be reported as stimulants.

- keepNecessaryStimulant:

  default set to FALSE. Specify if you want to distinguish necessary
  stimulants from stimulants.

- recordConjunctions:

  default set to FALSE. Specify if you want to record any conjunctions
  that appear in the network.
