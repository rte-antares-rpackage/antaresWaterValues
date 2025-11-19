# Post process water values

Replace extreme water values by chosen values.

## Usage

``` r
remove_out(
  results_dt,
  min = NULL,
  max = NULL,
  max_vu,
  min_vu,
  replace_na_method
)
```

## Arguments

- results_dt:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing water values. Output `watervalues` from
  [`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md).

- min:

  Double. Minimal accepted water value.

- max:

  Double. Maximal accepted water value.

- max_vu:

  Double. Value to use for water values higher than `max`.

- min_vu:

  Double. Value to use for water values lower than `min`.

- replace_na_method:

  Character. Method to replace extreme values, either
  `"constant values"` to replace by `max_vu` and `min_vu` or
  `"extreme values"` to replace by the extreme values of the current
  week.

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with same format than `results_dt`.
