# Modify time-series of clusters in fictive_area_bc to implement the constraint value for each week and each MC year

Modify time-series of clusters in fictive_area_bc to implement the
constraint value for each week and each MC year

## Usage

``` r
generate_rhs_bc(constraint_value, area, opts)
```

## Arguments

- constraint_value:

  Data.frame (week,sim,u)

- area:

  Area used to calculate watervalues

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
