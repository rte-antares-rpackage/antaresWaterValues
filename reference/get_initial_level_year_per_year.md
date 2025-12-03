# Get initial level year per year

Get initial level of hydro storage for the given area for each MC year
based on the scenario builder. If random in the scenario builder, the
initial level is defined trough low and high reservoir levels for the
first day.

## Usage

``` r
get_initial_level_year_per_year(area, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

Vector. Initial level for each MC year in percentage, value between 0
and 100%.
