# Launch the simulation

Launch the simulation

## Usage

``` r
launchSimulation(
  opts,
  i,
  sim_name,
  path_solver,
  expansion,
  show_output_on_console,
  constraint_value
)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- i:

  Integer. Number of simulation.

- sim_name:

  Character. The name of the simulation.

- path_solver:

  Character containing the Antares Solver path, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).

- expansion:

  Binary. True if mode expansion (ie linear relaxation) of Antares is
  used to run simulations, argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).
  It is recommended to use mode expansion, it will be faster (only one
  iteration is done) and results will be smoother as the cost result
  will correspond to the linear relaxation of the problem.

- show_output_on_console:

  Argument passed to
  [`runSimulation`](https://rte-antares-rpackage.github.io/antaresEditObject/reference/runSimulation.html).
