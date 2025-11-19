# Reset an Antares study. In case, there is a problem when executing `runWaterValuesSimulation()`, run this function to restore the study.

Reset an Antares study. In case, there is a problem when executing
[`runWaterValuesSimulation()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/runWaterValuesSimulation.md),
run this function to restore the study.

## Usage

``` r
resetStudy(opts, area, pumping, backup)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- area:

  Character. The Antares area concerned by water values computation.

- pumping:

  Boolean. True to take into account the pumping capacity.

- backup:

  List with hydro_storage, load and misc_gen backups for area generated
  by the function
  [`getBackupData()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/getBackupData.md)
