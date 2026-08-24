# A function to generate objects of class 'Hormone'

A function to generate objects of class 'Hormone'

## Slots

- `name`:

  the name of the hormone. Will be used as a tag to place node within
  the network.

- `container`:

  the location of action for the hormone. Either "scion", or
  "rootstock".

- `inputs`:

  data.frame with column names Node, Coregulator, Influence, and Delay.
  Influences can one of either "stimulation", "inhibition", "necessary
  stimulation", "necessary inhibition", "altSource" (in the case that
  some hormone is produced somewhere else and travels to the location),
  and "Unknown". Delay gives the interval of time by which the effect is
  delayed.

- `outputs`:

  data.frame with column names Node, Coregulator, and Influence.
  Influences can one of either "stimulation", "inhibition", "necessary
  stimulation", "necessary inhibition", "altSource" (in the case that
  some hormone is produced somewhere else and travels to the location),
  and "Unknown". Delay gives the interval of time by which the effect is
  delayed.

- `travel`:

  specifies if the hormone travels between compartments. Hormones travel
  from their current container, to the other. Is a numeric value
  specifying the rate of travel. Is 0 if there is no travel between
  compartments. Default for hormones traveling down is 1, and up is 0.8
  to reflect that signals moving up take a longer time than signals
  moving down.

- `degradation`:

  the rate at which the hormone degrades.

- `genotypes`:

  a vector with objects of class character. Lists the genotypes that are
  important to this node.

## Examples

``` r
NA
#> [1] NA
```
