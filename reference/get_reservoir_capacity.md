# Get reservoir capacity

Get hydro reservoir capacity for the given area. Reservoir management
must be on.

## Usage

``` r
get_reservoir_capacity(area, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

Double, reservoir capacity in MWh.
