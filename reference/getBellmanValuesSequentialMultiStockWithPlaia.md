# Calculate Bellman values sequentially, one area at a time.

For each area, reward functions are first computed using
[`calculateRewardsSimulationsWithPlaia()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/calculateRewardsSimulationsWithPlaia.md).
Bellman values are then computed with
[`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md),
and an optimal trajectory for the area is derived using
[`getOptimalTrend()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/getOptimalTrend.md).
The resulting trajectory is subsequently used to compute reward
functions for the next area in the sequence. For areas where Bellman
values have not yet been computed, either `initial_traj` or a short-term
trajectory is used as a fallback.

## Usage

``` r
getBellmanValuesSequentialMultiStockWithPlaia(
  list_areas,
  opts,
  nb_simulations,
  mcyears,
  penalty_low,
  penalty_high,
  states_step_ratio = 1/50,
  cvar_value = 1,
  penalty_final_level = NULL,
  initial_traj = NULL,
  list_areas_to_compute = NULL
)
```

## Arguments

- list_areas:

  Vector of areas concerned by simulations.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- nb_simulations:

  Number of controls to simulate

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

- penalty_low:

  Double. Penalty for violating the bottom rule curve, comparable to the
  unsupplied energy cost.

- penalty_high:

  Double. Penalty for violating the top rule curve, comparable to the
  spilled energy cost.

- states_step_ratio:

  Double. Discretization ratio to generate steps levels between the
  reservoir capacity and zero for which Bellman values are computed.

- cvar_value:

  Double from 0 to 1. The probability used in cvar method.

- penalty_final_level:

  Penalties (for both bottom and top rule curves) to force final level

- initial_traj:

  Initial trajectory (used for other storages)

- list_areas_to_compute:

  Vector of character. Areas for which to compute Bellman values. If
  `NULL`, all areas in `list_areas` are used.

## Value

List containing aggregated water values, reward functions and optimal
trajectories.
