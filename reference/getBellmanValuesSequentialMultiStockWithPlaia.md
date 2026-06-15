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
  list_final_level = NULL,
  initial_traj = NULL,
  list_areas_to_compute = NULL,
  cluster = "calin1",
  plaia_path = NULL,
  threads = 1L
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

- list_final_level:

  List of double. For each storage, final level (in percent between 0
  and 100) if final level is constrained. Initial level computed by
  [`get_initial_level()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_initial_level.md)
  by default.

- initial_traj:

  Initial trajectory (used for other storages)

- list_areas_to_compute:

  Vector of character. Areas for which to compute Bellman values. If
  `NULL`, all areas in `list_areas` are used.

- cluster:

  Character. Name of the cluster of antaresWeb

- plaia_path:

  Character. Path to the plaia executable. Required for local studies,
  ignored for API studies.

- threads:

  Integer. Number of threads used by the plaia executable (local studies
  only).

## Value

List containing aggregated water values, reward functions and optimal
trajectories.
