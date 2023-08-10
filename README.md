
# <img src="inst/images/antares_simulator.png" width="250" />

# Watervalues

The R package watervalues allows to :

1\. Prepare the Antares study by creating the fictive area,

2\. Launch the simulations by updating the coupling constraints,

3\. Read the results

4\. Calculate the water values

## Installation

You can install the package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rte-antares-rpackage/antaresWaterValues@light_version", build_vignettes = TRUE)
```

To install all the package dependencies you can run the script
`inst/dependencies.R`

## Using the Shiny app

``` r
library(watervalues)
#> Le chargement a nécessité le package : data.table
#> Le chargement a nécessité le package : shiny
#> Le chargement a nécessité le package : shinyBS
```

Now we are ready to use our package.

``` r
shiny_water_values()
```

![](inst/images/calculate_water_values.gif)

## Without the Shiny app

Begin by defining some parameters about your study.

``` r
study_path <- "your/path/to/the/antares/study"
area <- "name_of_area"
pumping <- T #T if pumping possible
mcyears <- 1:10 # Monte Carlo years you want to use

opts <- antaresRead::setSimulationPath(study_path,"input")
pump_eff <- getPumpEfficiency(area,opts=opts)
```

Then, you have to run simulations.

``` r
simulation_res <- runWaterValuesSimulation(
    area=area,
    nb_disc_stock = 5, #number of simulations
    nb_mcyears = mcyears,
    path_solver = "your/path/to/antares/bin/antares-8.1-solver.exe",
    fictive_area = paste0("watervalue_",area),
    thermal_cluster = "watervaluecluster",
    overwrite = TRUE,
    link_from=area,
    opts = opts,
    otp_dest=paste0(study_path,"/user"),
    file_name=paste0(j,"_",area), #name of the saving file
    pumping=pumping,
    efficiency=pump_eff,
    launch_simulations=T,
    reset_hydro=T
  )
```

It’s now possible to calculate water values.

``` r
results <- Grid_Matrix(
  area=area,
  simulation_names=simulation_res$simulation_names,
  simulation_values=simulation_res$simulation_values,
  nb_cycle = 2L,
  mcyears = mcyears,
  week_53 = 0,#initial Bellman values
  states_step_ratio = 1/20, # discretization of states
  method= c("mean-grid","grid-mean","quantile")[1],
  q_ratio=0.5,# for quantile method
  opts = opts,
  pumping=pumping,
  efficiency=pump_eff,
  correct_concavity = FALSE,#correct concavity of Bellman values
  correct_monotony_gain = FALSE,#correct monotony of rewards
  penalty_low = 3000,#penalty for bottom rule curve
  penalty_high = 3000,#penalty for top rule curve
  method_old_gain = T,# T if you want a simple linear interpolation of rewards,
                      # F if you want to use marginal price to interpolate
  hours_reward_calculation = c(seq.int(0,168,10),168),# used for marginal prices interpolation
  controls_reward_calculation = constraint_generator(area=area,
                                                     nb_disc_stock = 20,
                                                     pumping = pumping,
                                                     pumping_efficiency = pump_eff,
                                                     opts=opts)# used for marginal prices interpolation
)
```
