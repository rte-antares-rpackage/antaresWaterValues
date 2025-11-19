# Plot reward

Plot reward functions for different weeks listed in `weeks_to_plot` and
different scenarios listed in `scenarios_to_plot`.

## Usage

``` r
plot_reward_mc(reward_base, weeks_to_plot, scenarios_to_plot)
```

## Arguments

- reward_base:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing reward functions. Output `reward` from
  [`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md).

- weeks_to_plot:

  Vector of integer. Weeks to plot.

- scenarios_to_plot:

  Vector of integer. Scenarios to plot.

## Value

- graph:

  A
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

- table:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  containing plotted data.
