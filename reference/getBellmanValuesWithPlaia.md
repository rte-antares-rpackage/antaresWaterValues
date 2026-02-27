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
  force_final_level = TRUE,
  penalty_final_level = 0,
  penalty_low = 0,
  penalty_high = 0,
  cvar_value = 1,
  name_sim = "watervalues",
  n_controls = 51
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

- force_final_level:

  Binary. Whether final level should be constrained.

- penalty_final_level:

  Double. Penalties for both rule curves to constrain final level.

- penalty_low:

  Double. Penalty for violating the bottom rule curve, comparable to the
  unsupplied energy cost.

- penalty_high:

  Double. Penalty for violating the top rule curve, comparable to the
  spilled energy cost.

- cvar_value:

  Double from 0 to 1. The probability used in cvar method.

- name_sim:

  Character. Name of the simulation to launch

- n_controls:

  Integer. Number of controls to evaluate reward functions.

## Value

List of watervalues matrix
