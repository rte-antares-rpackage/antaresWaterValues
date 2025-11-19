# This function disable binding constraints for `runWaterValuesSimulation`

This function disable binding constraints for `runWaterValuesSimulation`

## Usage

``` r
disable_constraint(opts, pumping, area)
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- pumping:

  Boolean. True to take into account the pumping.

- area:

  Area used to calculate watervalues
