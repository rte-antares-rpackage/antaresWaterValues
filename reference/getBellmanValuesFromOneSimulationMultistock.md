# Compute Bellman values for several storage based on one simulation

First, an Antares simulation (based on short-term trajectories or
trajectories given by the simulation output of `simu`) is run. Marginal
prices and costs of this simulation are used to build univariate and
independent reward functions for each area with the function
[`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md)
with `method_old = F`. Finally, Bellman values and water values are
computed with
[`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md).
This method is fast and gives good results when storage are not too
important in the study. Otherwise, it is better to compute Bellman
values storage per storage.

## Usage

``` r
getBellmanValuesFromOneSimulationMultistock(
  opts,
  path_solver,
  mcyears,
  list_areas,
  list_pumping,
  list_efficiency,
  force_final_level,
  penalty_final_level_low,
  penalty_final_level_high,
  penalty_low,
  penalty_high,
  write_vu = F,
  simu = NULL
)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- path_solver:

  Character containing the Antares Solver path, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

- list_areas:

  Vector of areas concerned by simulations.

- list_pumping:

  Named vector of binary to tell if pumping is available in areas.

- list_efficiency:

  Named vector of pumping efficiency.

- force_final_level:

  Binary. Whether final level should be constrained.

- penalty_final_level_low:

  Double. Penalties for both bottom rule curve to constrain final level.

- penalty_final_level_high:

  Double. Penalties for top rule curve to constrain final level.

- penalty_low:

  Double. Penalty for violating the bottom rule curve, comparable to the
  unsupplied energy cost.

- penalty_high:

  Double. Penalty for violating the top rule curve, comparable to the
  spilled energy cost.

- write_vu:

  Binary. True to write water values in the Antares study.

- simu:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
  with the simulation selected from which to use the storage trajectory
  to run the simulation.

## Value

A
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
similar to `aggregated_results` from
[`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md)
with one additional column `"area"`.
