# Change hydro management

For the given area, choose if hydro storage is managed with water values
and/or heuristic. It is not possible to have neither water values nor
heuristic.

## Usage

``` r
changeHydroManagement(watervalues = FALSE, heuristic = TRUE, opts, area)
```

## Arguments

- watervalues:

  Binary. TRUE if use water values.

- heuristic:

  Binary. TRUE if use heuristic.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- area:

  Character. Antares area for which to change hydro management.
