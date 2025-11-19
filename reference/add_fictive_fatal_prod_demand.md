# Add fictive production and fictive load to avoid infeasabilities with binding constraints

Add fictive production and fictive load to avoid infeasabilities with
binding constraints

## Usage

``` r
add_fictive_fatal_prod_demand(area, opts, load, misc_gen)
```

## Arguments

- area:

  A valid Antares area.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- load:

  Matrix with 8760 rows that contains backup load for the area

- misc_gen:

  Matrix with 8760 rows that contains backup misc generation for the
  area

## Value

An updated list containing various information about the simulation.
