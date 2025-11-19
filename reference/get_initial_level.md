# Get initial level

Get initial level of hydro storage for the given area. Initial level is
defined trough low and high reservoir levels for the first day.

## Usage

``` r
get_initial_level(area, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

Double. Initial level in percentage, value between 0 and 100%.
