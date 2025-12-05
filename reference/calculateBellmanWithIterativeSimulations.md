# Calculate Bellman values throughout iterations of Antares simulation and DP Each simulation leads to a new reward estimation, which leads to new water values, which leads to the off-line calculation in R of an optimal trajectory, which leads to new controls to be evaluated which leads to a new simulation

Calculate Bellman values throughout iterations of Antares simulation and
DP Each simulation leads to a new reward estimation, which leads to new
water values, which leads to the off-line calculation in R of an optimal
trajectory, which leads to new controls to be evaluated which leads to a
new simulation

## Usage

``` r
calculateBellmanWithIterativeSimulations(
  area,
  pumping,
  pump_eff = 1,
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
  df_previous_cut = NULL
)
```

## Arguments

- area:

  Area with the reservoir

- pumping:

  Binary, TRUE if pumping is possible

- pump_eff:

  Pumping efficiency (1 if no pumping)

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

  from 0 to 1. the probability used in cvar method

- penalty_final_level:

  Penalties (for both bottom and top rule curves) to constrain final
  level

- df_previous_cut:

  Data frame containing previous estimations of cuts

## Value

List containing aggregated water values and the data table with all
years for the last iteration
