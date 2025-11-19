# Generate backup data for area

Generate backup data for area

## Usage

``` r
getBackupData(area, mcyears, opts)
```

## Arguments

- area:

  Character. The Antares area concerned by water values computation.

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

## Value

List with hydro_storage, load and misc_gen backups for area
