# Get pumping and generating hydro power

Get maximum pumping and generating hydro capacities over a given horizon
given in `timeStep`.

## Usage

``` r
get_max_hydro(area, opts, timeStep = "hourly")
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- timeStep:

  Character among "hourly", "daily" and "weekly".

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 3 columns : `"timeId"`, `"pump"` and `"turb"`.
