# Compute optimal candidates for H2 system

Search optimal solution for bounded candidates for the H2 systems
including storage, must-run clusters and flexibles clusters.

## Usage

``` r
MultiStock_H2_Investment_reward_compute_once(
  areas_invest,
  max_ite,
  storage_bounds,
  storage_points_nb,
  candidates_types_gen,
  penalty_low = 5000,
  penalty_high = 5000,
  penalty_final_level = 5000,
  opts,
  mc_years_optim,
  path_to_antares,
  cvar = 1,
  storage_annual_cost,
  launch_sims = T,
  nb_sims = 51,
  parallelprocess = F,
  nb_sockets = 0,
  unspil_cost = 3000,
  edit_study = F,
  file_intermediate_results,
  back_to_first_node = F
)
```

## Arguments

- areas_invest:

  Vector of characters of the names of areas to optimize.

- max_ite:

  Integer. Maximum number of iterations for each area.

- storage_bounds:

  Vector of integers of length 2, with the form (min, max).

- storage_points_nb:

  Integer. Number of storage points to test at each iteration. Must be
  \>3 to update bounds at each iterations and approach solution.

- candidates_types_gen:

  Data_frame with column names :
  `c(index, name, type, TOTEX, Marg_price, Part_fixe, Prix_fixe, Borne_min, Borne_max, Points_nb, Zone)`.
  Each row describes a cluster candidate. The index should correspond to
  the index of the candidate in `candidates_data`. The name is a
  character, the type is either `"cluster_flexible"` or
  `"cluster_bande"`, TOTEX is in eur/MW/year, Marg_price is in eur/MWh.
  Part_fixe is the fixed part for a variable cluster between 0 and 1,
  Prix_fixe in eur/MWh is its price. Borne_min and Borne_max are in MW,
  Points_nb is an integer (number of candidates tested at each
  iteration, it should be at least 4). Zone is a character containing
  the name of the area where to propose the candidate.

- penalty_low:

  Integer. Penalty for lower guide curve.

- penalty_high:

  Integer. Penalty for higher guide curve.

- penalty_final_level:

  Integer. Penalty for higher and lower final level.

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- mc_years_optim:

  Vector of integers. Monte Carlo years to perform the optimization.

- path_to_antares:

  Character containing the Antares Solver path, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).

- cvar:

  Numeric in \[0,1\]. The probability used in cvar algorithm.

- storage_annual_cost:

  Numeric. Annual cost of storage in eur/MWh.

- launch_sims:

  Boolean. True to launch simulations at each iterations. False if
  simulations already run.

- nb_sims:

  Integer. Number of simulations to launch to evaluate reward.

- parallelprocess:

  Boolean. True to compute Water values with parallel processing.

- nb_sockets:

  Integer. Number of sockets for parallel processing

- unspil_cost:

  Numeric. Unspilled energy cost in eur/MW for all concerned areas.

- edit_study:

  Boolean. True to edit study with optimal candidates.

- file_intermediate_results:

  Character. Local path to save intermediate results.

- back_to_first_node:

  Boolean. True to play again first node at the end. There is no
  possibility to go uninvest.

## Value

a `list` containing for each area detailed results (best candidate, all
total costs, reward function, optimization time)
