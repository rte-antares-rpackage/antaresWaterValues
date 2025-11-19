# Get pumping efficiency ratio

Get pumping efficiency ratio for the given area. Reservoir management
must be on.

## Usage

``` r
getPumpEfficiency(area, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

Double. Pumping efficiency ratio.
