# Plot Bellman and water values

Plot Bellman values and water values for differents weeks listed in
`weeks_to_plot`.

## Usage

``` r
plot_Bellman(value_nodes_dt, weeks_to_plot)
```

## Arguments

- value_nodes_dt:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing the Bellman and water values. Output `aggregated_results`
  from
  [`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md).

- weeks_to_plot:

  Vector of integer. Weeks to plot.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
