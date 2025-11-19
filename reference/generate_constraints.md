# This function generate binding constraints for `runWaterValuesSimulation`

This function generate binding constraints for
`runWaterValuesSimulation`

## Usage

``` r
generate_constraints(pumping, efficiency, opts, area)
```

## Arguments

- pumping:

  bool

- efficiency:

  in \[0,1\]. efficient ratio of pumping.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- area:

  Area used to calculate watervalues
