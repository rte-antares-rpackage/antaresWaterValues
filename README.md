[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresWaterValues.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresWaterValues)

# antaresWaterValues


> Calculate Water Values.





## Running water values simulation


```r
library( antaresRead )
library( antaresEditObject )
library( antaresWaterValues )

# Set simulation path to an Antares study in Input mode
setSimulationPath(path = "inputs/BP16_2020_conso_median_defav/", simulation = "input")

# For speed, we limit the number of MC years to 20
updateGeneralSettings(nbyears = 20)


# Run several simulation with different constraint values

# here 15 simulations with binding constraints values from 0 to 1.344 (max hydro storage on a week)

# This function will run Antares with parameters specified for calculate water values
# (create a fictive area, a link between the area studied and the fictive area,
# a binding constraints, set to 0 the hydro storage times series, ...)
# and return the names of the simulations run

simulation_names <- runWaterValuesSimulation(
  area = "fr",
  nb_simulation = 15,
  path_solver = "C:/antares/bin/antares-6.0-solver.exe", 
  show_output_on_console = FALSE,
  overwrite = TRUE
)
```

When each simulation has run, we can use result to calculate a mean grid layer :


```r
# Parameters

# Values for simulations
simulation_values <- gsub("weekly_water_amount_", "", simulation_names)
simulation_values <- gsub(",", ".", simulation_values)
simulation_values <- as.numeric(simulation_values)

# Number of weeks
n_weeks <- 53

# states matrix
states <- matrix( rep(seq(from = 10, to = 0, by = -0.05), n_weeks), byrow = FALSE, ncol = n_weeks)



# Nodes values calculation
value_nodes <- meanGridLayer(
  area = "fr",
  simulation_names = simulation_names, 
  simulation_values = simulation_values,
  states = states, max_mcyears = 10,
  n_week = n_weeks, 
  week_53 = 0
)
```

Result is a `data.table` with 3 columns, the number of the weeks, the levels of states and the nodes values.

You can tranform this table in a matrix states X weeks like this :

```
library(data.table)
value_nodes_dc <- dcast(
  data = value_nodes, 
  formula = statesid ~ weeks, 
  value.var = "value_node"
)
```

Here's a representation of the result :

```r
library(ggplot2)
ggplot(data = value_nodes) + 
  aes(x = weeks, y = rev(statesid), fill = value_node) + 
  geom_tile() + 
  scale_fill_viridis(na.value = "transparent") + 
  theme_minimal() + 
  labs(title = "Mean Grid Layer", x = "Weeks", y = "States")

```
![mean grid layer](inst/img/mean_grid_layer.png)


