# Setup a simulation before running it for calculating Water Values, used in `runWaterValuesSimulation`

Setup a simulation before running it for calculating Water Values, used
in `runWaterValuesSimulation`

## Usage

``` r
setupWaterValuesSimulation(
  area,
  overwrite = FALSE,
  opts,
  pumping = FALSE,
  efficiency,
  backup
)
```

## Arguments

- area:

  The area concerned by the simulation.

- overwrite:

  If area or cluster already exists, overwrite them ?

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- pumping:

  Boolean. True to take into account the pumping.

- efficiency:

  Double. Pumping efficiency

- backup:

  List with hydro_storage, load and misc_gen backups for area generated
  by the function `getBackupData`

## Value

The result of antaresRead::simOptions
