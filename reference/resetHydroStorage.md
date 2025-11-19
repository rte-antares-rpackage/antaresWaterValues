# Reset to 0 the hydro storage time series, used in `setupWaterValuesSimulation`

Reset to 0 the hydro storage time series, used in
`setupWaterValuesSimulation`

## Usage

``` r
resetHydroStorage(area, opts)
```

## Arguments

- area:

  A valid Antares area.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

You can restore the original hydro storage time series with
`restoreHydroStorage`.

## See also

[restoreHydroStorage](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/restoreHydroStorage.md)
