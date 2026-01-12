# Compute reward function with the 5 simulations method. Called for each area of `MultiStock_H2_Investment_reward_compute_once`. If they are several areas, the trajectories of the other storage are fixed on their optimal trend.

Compute reward function with the 5 simulations method. Called for each
area of `MultiStock_H2_Investment_reward_compute_once`. If they are
several areas, the trajectories of the other storage are fixed on their
optimal trend.

## Usage

``` r
calculateRewardsSimulations(
  node,
  list_areas,
  list_efficiency,
  opts,
  mcyears,
  prefix,
  sim_number = 5,
  optimal_traj,
  max_power_invest,
  list_max_hydro
)
```

## Arguments

- node:

  Character. Name of the area where the reward is computed

- list_areas:

  Vector of characters of the names of areas to optimize.

- list_efficiency:

  Vector of numeric with pumping efficiency for the area with
  corresponding index in `list_areas`.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- mcyears:

  Vector of integers. Monte Carlo years to run simulations

- prefix:

  Character. Prefix of the simulation.

- sim_number:

  Integer. Number of simulations.

- optimal_traj:

  Data frame containing optimal trajectory for all areas

- max_power_invest:

  Double. Sum of upper bounds of candidates for the node.

- list_max_hydro:

  List of vectors. Giving max generating capacity and pumping capacity
  for each area.

## Value

a `data_frame` containing the rewards returned by the function
[`antaresWaterValues::get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md)
