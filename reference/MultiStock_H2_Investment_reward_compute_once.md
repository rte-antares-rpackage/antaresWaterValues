# Compute Optimal Investment Candidates for H2 Systems

Searches for the optimal investment solution among bounded candidates
for H2 systems, including storage, must-run clusters, and flexible
clusters.

## Usage

``` r
MultiStock_H2_Investment_reward_compute_once(
  areas_invest,
  max_ite,
  list_storage_bounds,
  storage_points_nb,
  candidates_types_gen,
  penalty_low = 5000,
  penalty_high = 5000,
  penalty_final_level = 5000,
  opts,
  mc_years_optim,
  cvar = 1,
  storage_annual_cost,
  nb_sims = 51,
  parallelprocess = F,
  nb_sockets = 0,
  unspil_cost = 3000,
  file_intermediate_results,
  list_ratio_max_hydro,
  remove_cluster = F
)
```

## Arguments

- areas_invest:

  \`character vector\` Names of the areas/zones to optimize.

- max_ite:

  \`integer\` Maximum number of iterations per area.

- list_storage_bounds:

  \`list\` List of vectors of two integers (min, max) for each area,
  specifying the storage bounds to test.

- storage_points_nb:

  \`integer\` Number of storage points to test at each iteration (must
  be \> 3 for effective refinement).

- candidates_types_gen:

  \`data.frame\` A data.frame describing the generation candidates to be
  optimized. \*\*See section ‘Structure of candidates_types_gen’ for
  details about required columns and their usage.\*\*

- penalty_low:

  \`numeric\` Penalty when storage is below the guide curve (default:
  5000).

- penalty_high:

  \`numeric\` Penalty when storage exceeds the guide curve (default:
  5000).

- penalty_final_level:

  \`numeric\` Penalty for out-of-bounds final storage level (default:
  5000).

- opts:

  \`list\` Study parameters (typically the output of
  \`antaresRead::setSimulationPath(simulation = "input")\`).

- mc_years_optim:

  \`integer vector\` Monte Carlo years used for optimization.

- cvar:

  \`numeric \[0,1\]\` Probability used in \`Grid_Matrix()\` (default:
  1).

- storage_annual_cost:

  \`numeric\` Annual storage cost in €/MWh.

- nb_sims:

  \`integer\` Number of simulations per evaluation (default: 51).

- parallelprocess:

  \`logical\` Whether to use parallel processing for Water values
  calculations.

- nb_sockets:

  \`integer\` Number of sockets to use if \`parallelprocess = TRUE\`.

- unspil_cost:

  \`numeric\` Unspilled energy cost in €/MW for all relevant areas.

- file_intermediate_results:

  \`character\` Local path for saving intermediate results for
  backup/restart.

- list_ratio_max_hydro:

  \`list\` For each area, gives the ratio used to deduce maximum weekly
  power for the dimensioning (used at weekly resolution only).

- remove_cluster:

  \`logical\` Should candidate clusters be removed before running the
  investment process.

## Value

A list, with each element corresponding to an optimized area containing:

- \`all_costs\`: Complete grid of total costs for all tested candidates,

- \`best\`: The best candidate(s) found,

- \`last_storage_points\`, \`last_candidates_data\`: Bounds and
  parameters used in the final iteration,

- \`optim_time\`: Optimization duration,

- \`reward\`: Reward function used,

- \`optimal_traj\`: Optimal storage trajectory,

- \`optimal_max_hydro\`, \`watervalues\`: Optimal hydraulic parameters,
  etc.

## Details

The function first computes gain (reward) functions using the 'plaia'
methodology (via Monte Carlo simulations). For each area, it then
iterates over all candidate combinations, calculating the operational
cost for each using dynamic programming (\`Grid_Matrix()\`). The
operational cost computation is based on optimal use (value functions)
that are themselves modified by each candidate's characteristics
(storage size, cluster types, etc.). At each iteration, the tested
domain is refined around the best candidate solution from the previous
iteration (grid refinement).

The function optimizes the \*\*installed power\*\* (e.g.
turbine/pump/generator capacities) of candidates, but only to determine
the \*\*maximum weekly energy or power\*\* that can be delivered or
absorbed. All constraints and optimizations are performed on a weekly
basis. \*\*Hourly power-related congestion or inadequacies cannot be
detected\*\*: only weekly energy or power limitations are modeled in the
optimization. This means that the function is suited for weekly energy
sizing, but will not capture operational issues that arise at an hourly
timescale.

## Note

Power sizing is optimized, but for weekly assessment only. If you need
to capture possible hourly power congestion or short-term operating
constraints, you will need a different or more detailed optimization.

## Structure of candidates_types_gen

The `candidates_types_gen` argument must be a data.frame with at least
the following columns:

- index:

  Integer. Unique identifier for each candidate (should be 1, 2, ...,
  n).

- name:

  Character. Name of the candidate (used for study and result
  traceability).

- type:

  Character. Type of cluster: either \`"cluster_flexible"\`
  (dispatchable/flexible) or \`"cluster_bande"\` (must-run or base
  load).

- TOTEX:

  Numeric. Total annualized cost per MW of the candidate
  (euros/MW/year).

- Marg_price:

  Numeric. Marginal cost for the candidate (euros/MWh).

- Part_fixe:

  Numeric, between 0 and 1. Fixed part of a flexible cluster (only for
  \`type == "cluster_flexible"\`). Value of 0 if not applicable.

- Prix_fixe:

  Numeric. Fixed price (euros/MWh) for the fixed part of a flexible
  cluster.

- Borne_min:

  Numeric. Minimum possible size (MW) for the candidate.

- Borne_max:

  Numeric. Maximum possible size (MW) for the candidate.

- Points_nb:

  Integer (\\=4). Number of tested candidate positions (grid points) for
  this cluster per iteration.

- Zone:

  Character. Name of the area/zone where this candidate can be added.

## Notes

- Flexible clusters are automatically split into fixed and variable
  parts if needed.

- Grid ranges are updated at each iteration based on the best found
  solution (refinement approach).

- The function manages Monte Carlo simulations and ensures
  reproducibility via the intermediate result file.

- Only weekly (not hourly) congestion or adequacy issues can be
  assessed.
