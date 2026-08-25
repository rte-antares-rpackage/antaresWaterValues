# Validate a vector of area names against the study and normalize their case.

Validate a vector of area names against the study and normalize their
case.

## Usage

``` r
validate_and_normalize_areas(list_areas, opts)
```

## Arguments

- list_areas:

  Character vector of area names.

- opts:

  List. Antares simulation options, as returned by
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Value

Character vector of `list_areas` normalized to lower case.
