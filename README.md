
<img src="vignettes/images/antares_simulator.png" align="right" width=250 />
<br/>

# Watervalues

The R package watervalues allows to :
Prepare the Antares study by creating the fictive area
Launch the simulations by updating the coupling constraints
Read the results and calculate the water values

## Install package


You can install the package from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("dhia-gharsallaoui/watervalues", build_vignettes = TRUE)
```


## Load the package
```{r Load the package, eval=TRUE}
library(watervalues)
```


Now we are ready to use our package.

## Set the study
Use the package antaresRead to select the study in Input mode.
```{r warning=FALSE}
 library(antaresRead)
 study_path <- "D:/Users/gharsallaouidhi/Documents/20200722_MAF2025_S4_V2_7.1"
 opts <- antaresRead::setSimulationPath(path = study_path, simulation = "input")
```

## Start simulation
To generate the Reward Matrix of transitions, it's necessary to run a certain number of 
simulations *nb_disc_stock*. The larger this number is, the more precise rewards we get.

These simulations are launched from R with the `runWaterValuesSimulation()` function and are run by the Antares solver.
*NB:* A large number will cause a long time of simulation.

Skip this step if you already do it before and you have the Rdata file.

```{r eval=FALSE}
# simulation_res <- runWaterValuesSimulation(
#   area = "2_nom1_hydro_open",                        # name of area
#   simulation_name = "nom_def0_weekly_water_amount_%s",
#   path_solver="D:/Users/gharsallaouidhi/Documents/antares/rte-antares-8.0.3-installer-64bits/bin/antares-8.0-solver.exe",                 # solver path 
#   fictive_area = "watervalues_fr",    # name of the Fictive created area 
#   nb_disc_stock = n,                  # number of simulations
#   overwrite = TRUE,
#   nb_mcyears=15
# )
# save(simulation_res,"simulation_res.Rdata")
```
Save the output file in Rdata file to avoid losing the simulation information for the future calculation.

 * `area` is the relevant area.
 * `nb_mcyears` is the number of Monte-carlo year that will be generated. Here we
 limit it to 10 to increase speed.
 * `path_solver` is the path to the Antares solver.
 * `nb_disc_stock` is the number of values used to discretize the stock level between
 0 and the maximum value for the given area. Here 10 simulations are run, with 10
 binding constraints ranging from 0 to the maximum of hydro
 energy that can be produced over a week.
 * `overwrite` allows to overwrite the area, cluster and binding constraint if they
 already exist.
 
## Calculate Watervalues

 
Water values are generated in R with the `Grid_Matrx()` function, using the results
of the above simulations.
But we implemented all the scripts in a Shiny web App to facilitate the use of the package.
```{r fig.height=7, fig.width=7, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE}

shiny_Grid_matrix(simulation_res,opts)


```
 ![tutorial](https://github.com/dhia-gharsallaoui/watervalues/blob/main/vignettes/images/calculate%20water%20values.gif?raw=true)
 
 * `area` is the relevant area.
 * `water value initial condition` are the water values for the last week of the year.
 * `nb_cycle` is the number of times to run the algorithm in a loop from the last
 week of the year to the first, in order to obtain more realistic start values for 
 week 53.
 * `method` is the algorithm to be used to calculate Bellman values. We have three algorithms for the moment
 tap `help(Grid_Matrix)` to learn about them.
The result is a 7-column `data.table`: Bellman values are in the `value_node` column,
and water values in the `vu` column.

