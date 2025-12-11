# Get objective values of the optimization problem of each week and each scenario for a given simulation `simu`, mainly used in `get_Reward()` to build reward functions.

Get objective values of the optimization problem of each week and each
scenario for a given simulation `simu`, mainly used in
[`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md)
to build reward functions.

## Usage

``` r
get_weekly_cost(simu, mcyears, expansion = F)
```

## Arguments

- simu:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
  with the simulation selected.

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

- expansion:

  Binary. True if mode expansion (ie linear relaxation) of Antares is
  used to run simulations, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).
  It is recommended to use mode expansion, it will be faster (only one
  iteration is done) and results will be smoother as the cost result
  will correspond to the linear relaxation of the problem.

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 3 columns : `"timeId"`, `"mcYear"` and `"ov_cost"`.
