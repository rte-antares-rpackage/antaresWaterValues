# Calculate Bellman values throughout iterations of Antares simulation and DP Each simulation leads to a new reward estimation, which leads to new water values, which leads to the off-line calculation in R of an optimal trajectory, which leads to new controls to be evaluated which leads to a new simulation

Calculate Bellman values throughout iterations of Antares simulation and
DP Each simulation leads to a new reward estimation, which leads to new
water values, which leads to the off-line calculation in R of an optimal
trajectory, which leads to new controls to be evaluated which leads to a
new simulation

## Usage

``` r
calculateBellmanWithIterativeSimulationsMultiStock(
  list_areas,
  list_pumping,
  list_efficiency,
  opts,
  nb_control = 10,
  nb_itr = 3,
  mcyears,
  penalty_low,
  penalty_high,
  path_solver,
  states_step_ratio = 1/50,
  cvar_value = 1,
  penalty_final_level = NULL,
  initial_traj = NULL,
  df_previous_cut = NULL,
  list_areas_to_compute = NULL
)
```

## Arguments

- list_areas:

  Vector of areas concerned by simulations.

- list_pumping:

  Named vector of binary to tell if pumping is available in areas.

- list_efficiency:

  Named vector of pumping efficiency.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- nb_control:

  Number of controls used in the interpolation of the reward function

- nb_itr:

  Max number of iterations

- mcyears:

  Vector of years used to evaluate rewards

- penalty_low:

  Penalty for violating the bottom rule curve, comparable to the
  unsupplied energy cost

- penalty_high:

  Penalty for violating the top rule curve, comparable to the spilled
  energy cost

- path_solver:

  Character containing the Antares Solver path, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).

- states_step_ratio:

  Discretization ratio to generate steps levels between the reservoir
  capacity and zero

- cvar_value:

  from 0 to 1. the probability used in quantile method to determine a
  bellman value which cvar_value all bellman values are equal or less to
  it. (quantile(cvar_value))

- penalty_final_level:

  Penalties (for both bottom and top rule curves) to constrain final
  level

- initial_traj:

  Initial trajectory (used for other storages)

- df_previous_cut:

  Data frame containing previous estimations of cuts

- list_areas_to_compute:

  Vector of character. Areas for which to compute Bellman values. If
  `NULL`, all areas in `list_areas` are used.

## Value

List containing aggregated water values and the data table with all
years for the last iteration
