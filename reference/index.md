# Package index

## All functions

- [`Grid_Matrix()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/Grid_Matrix.md)
  : Compute Bellman values

- [`calculateBellmanWithIterativeSimulations()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/calculateBellmanWithIterativeSimulations.md)
  : Calculate Bellman values throughout iterations of Antares simulation
  and DP Each simulation leads to a new reward estimation, which leads
  to new water values, which leads to the off-line calculation in R of
  an optimal trajectory, which leads to new controls to be evaluated
  which leads to a new simulation

- [`calculateBellmanWithIterativeSimulationsMultiStock()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/calculateBellmanWithIterativeSimulationsMultiStock.md)
  : Calculate Bellman values throughout iterations of Antares simulation
  and DP Each simulation leads to a new reward estimation, which leads
  to new water values, which leads to the off-line calculation in R of
  an optimal trajectory, which leads to new controls to be evaluated
  which leads to a new simulation

- [`changeHydroManagement()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/changeHydroManagement.md)
  : Change hydro management

- [`constraint_generator()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/constraint_generator.md)
  : Generate control values

- [`getBellmanValuesFromOneSimulationMultistock()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/getBellmanValuesFromOneSimulationMultistock.md)
  : Compute Bellman values for several storage based on one simulation

- [`getOptimalTrend()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/getOptimalTrend.md)
  :

  Calculate an optimal trajectory for the reservoir levels based on
  water values taking into account the mean inflow, used in
  `calculateBellmanWithIterativeSimulations`

- [`getPumpEfficiency()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/getPumpEfficiency.md)
  : Get pumping efficiency ratio

- [`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md)
  : Compute reward functions

- [`get_inflow()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_inflow.md)
  : Get weekly inflows

- [`get_initial_level()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_initial_level.md)
  : Get initial level

- [`get_initial_level_year_per_year()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_initial_level_year_per_year.md)
  : Get initial level year per year

- [`get_local_reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_local_reward.md)
  : Compute reward functions based on one given simulation

- [`get_max_hydro()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_max_hydro.md)
  : Get pumping and generating hydro power

- [`get_reservoir_capacity()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_reservoir_capacity.md)
  : Get reservoir capacity

- [`get_weekly_cost()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_weekly_cost.md)
  :

  Get objective values of the optimization problem of each week and each
  scenario for a given simulation `simu`, mainly used in
  [`get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md)
  to build reward functions.

- [`plot_Bellman()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/plot_Bellman.md)
  : Plot Bellman and water values

- [`plot_reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/plot_reward.md)
  : Plot mean reward

- [`plot_reward_mc()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/plot_reward_mc.md)
  : Plot reward

- [`plot_reward_variation()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/plot_reward_variation.md)
  : Plot mean reward variation

- [`plot_reward_variation_mc()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/plot_reward_variation_mc.md)
  : Plot reward variation

- [`readReservoirLevels()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/readReservoirLevels.md)
  : Read reservoir rule curves

- [`remove_out()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/remove_out.md)
  : Post process water values

- [`reward_offset()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/reward_offset.md)
  : Take into account overall cost in local reward

- [`runWaterValuesSimulation()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/runWaterValuesSimulation.md)
  : Run Antares simulations in order to compute water values for a
  specific area

- [`runWaterValuesSimulationMultiStock()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/runWaterValuesSimulationMultiStock.md)
  : Run Antares simulations in order to compute water values for
  multiple areas

- [`shiny_water_values()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/shiny_water_values.md)
  : Open web interface for computing water values

- [`to_Antares_Format()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/to_Antares_Format.md)
  : Convert water values to Antares format

- [`to_Antares_Format_bis()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/to_Antares_Format_bis.md)
  : Convert water values to Antares format with high accuracy

- [`updateReward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/updateReward.md)
  :

  Update df_rewards with latest simulation run, used in
  `calculateBellmanWithIterativeSimulations`

- [`updateWatervalues()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/updateWatervalues.md)
  :

  Calculate water values with `Grid_Matrix` from estimated reward, used
  in `calculateBellmanWithIterativeSimulations`

- [`waterValuesViz()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/waterValuesViz.md)
  : Plot water values
