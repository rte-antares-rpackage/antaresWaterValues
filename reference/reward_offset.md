# Take into account overall cost in local reward

Modify local reward to take into account overall cost of the simulation,
mainly used in
[`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md).

## Usage

``` r
reward_offset(simu, df_reward, u0 = c(), mcyears, expansion = F)
```

## Arguments

- simu:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
  with the simulation selected.

- df_reward:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  computed by the function
  [`get_local_reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_local_reward.md).

- u0:

  A
  [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with columns `"week"`, `"u"` and `"mcYear"` (optional) that gives
  constraint values per week (and per scenario) used in the simulation.
  Could be extracted from the `simulation_values` output of
  [`runWaterValuesSimulation()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/runWaterValuesSimulation.md)
  by filtering column `"sim"` to the corresponding simulation.

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
with columns `"week"`, `"mcYear"`, `"u"` and `"reward"`.
