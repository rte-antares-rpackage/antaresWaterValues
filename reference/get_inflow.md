# Get weekly inflows

Get weekly inflows of the given area for all weeks from 1 to 52 and for
all time-series in `mcyears`. Please note that if time series index
don't correspond to scenario number in scenario builder, this could
generate inconsistencies.

## Usage

``` r
get_inflow(area, opts, mcyears)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 4 columns : `"timeId"` (integer), `"tsId"` (integer),
`"hydroStorage"` (double) and `"area"` (character). Each line give the
amount of weekly inflow (`"hydroStorage"`) for `area` for a week
(`"timeId"`) and a scenario (`"tsId"`).
