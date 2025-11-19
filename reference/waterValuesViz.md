# Plot water values

Plot water values for all weeks and all levels of stock.

## Usage

``` r
waterValuesViz(Data, filter_penalties = FALSE)
```

## Arguments

- Data:

  Water values result. Output `aggregated_results` from
  [`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md).

- filter_penalties:

  Binary. If `TRUE`, show only water values inside rule curves.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
