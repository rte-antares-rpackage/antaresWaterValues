# Compute Bellman values for several storage with plaia

For each storage, reward functions are evaluated and Bellman values are
computed with plaia. Results are written directly to the output folder.
Final level is equal to initial level for all storages.

## Usage

``` r
getBellmanValuesWithPlaia(
  opts,
  mcyears,
  list_areas,
  list_force_final_level = list(),
  list_final_level = list(),
  list_penalty_final_level = list(),
  list_penalty_low = list(),
  list_penalty_high = list(),
  list_cvar_value = list(),
  name_sim = "watervalues",
  n_controls = 51,
  n_levels = 101,
  cluster = "calin1",
  plaia_path = NULL,
  solver = "xpress",
  threads = 1L
)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- mcyears:

  Vector of integer. Monte Carlo years used to compute water values.

- list_areas:

  Vector of areas concerned by simulations.

- list_force_final_level:

  Named list of logical. Per-area flag indicating whether the final
  level should be constrained.

- list_final_level:

  Named list of double. Per-area final reservoir level (in percent
  between 0 and 100). Used when `list_force_final_level` is `TRUE`.

- list_penalty_final_level:

  Named list of double. Per-area penalties for both rule curves to
  constrain final level.

- list_penalty_low:

  Named list of double. Per-area penalty for violating the bottom rule
  curve, comparable to the unsupplied energy cost.

- list_penalty_high:

  Named list of double. Per-area penalty for violating the top rule
  curve, comparable to the spilled energy cost.

- list_cvar_value:

  Named list of double (values between 0 and 1). Per-area probability
  used in the CVaR method.

- name_sim:

  Character. Name of the simulation to launch

- n_controls:

  Integer. Number of controls to evaluate reward functions.

- n_levels:

  Integer. Number of Bellman value levels to compute.

- cluster:

  Character. Name of the cluster of antaresWeb

- plaia_path:

  Character. Path to the plaia executable. Required for local studies,
  ignored for API studies.

- solver:

  Character. Solver used by plaia (e.g. `"xpress"`, `"coin"`).

- threads:

  Integer. Number of threads used by the plaia executable (local studies
  only).

## Value

List of watervalues matrix
