# Restore the hydro storage time series, used in `runWaterValuesSimulation`

Restore the hydro storage time series, used in
`runWaterValuesSimulation`

## Usage

``` r
restoreHydroStorage(area, opts, data)
```

## Arguments

- area:

  A valid Antares area.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- data:

  Backup hydro storage matrix

## Value

An updated list containing various information about the simulation.
