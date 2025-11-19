# Read reservoir rule curves

Read reservoir low and high levels of hydro object of the specified
area.

## Usage

``` r
readReservoirLevels(area, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 4 columns : `"timeId"` (index of the week), `"level_low"`,
`"level_avg"` and `"level_high"`. Values are given for the end of the
last hour of the end of the week.
