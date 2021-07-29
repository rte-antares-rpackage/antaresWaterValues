#' Run a simulation for calculating water values for a specific area
#'
#' @param area The area concerned by the simulation.
#' @param simulation_name The name of the simulation, \code{s} is a placeholder for the constraint value defined by \code{nb_disc_stock}.
#' @param nb_disc_stock Number of simulation to launch, a vector of energy constraint
#'  will be created from 0 to the hydro storage maximum and of length this parameter.
#' @param nb_mcyears Number of Monte Carlo years to simulate.
#' @param binding_constraint Name of the binding constraint.
# @param constraint_values Vector of energy constraints on the link between the area and the fictive area.
#' @param fictive_area Name of the fictive area to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param thermal_cluster Name of the thermal cluster to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param path_solver Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param wait Argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param show_output_on_console Argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param overwrite If area or cluster already exists, should they be overwritten?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @note This function have side effects on the Antares study used, a fictive area is created and a new district as well.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresEditObject createBindingConstraint removeBindingConstraint readIniFile writeIni runSimulation
#' @importFrom antaresRead readClusterDesc
#' @importFrom stats setNames
#'

runWaterValuesSimulation <- function(area,
                                     simulation_name = "weekly_water_amount_%s",
                                     nb_disc_stock = 10,
                                     nb_mcyears = NULL,
                                     binding_constraint = "WeeklyWaterAmount",
                                     # constraint_values,
                                     fictive_area = NULL,
                                     thermal_cluster = NULL,
                                     path_solver,
                                     wait = TRUE,
                                     show_output_on_console = FALSE,
                                     overwrite = FALSE,
                                     opts = antaresRead::simOptions()) {



#set study

antaresRead::setSimulationPath(path = study, simulation = "input")


#generating the fictive area parameters

assertthat::assert_that(class(opts) == "simOptions")


fictive_area <- if (!is.null(fictive_area)) fictive_area else paste0("watervalue_", area)
thermal_cluster <- if (!is.null(thermal_cluster)) thermal_cluster else "WaterValueCluster"

if (!is.null(nb_mcyears)) {
  updateGeneralSettings(nbyears = nb_mcyears, opts = opts)
  }


#create the fictive area

opts <- setupWaterValuesSimulation(
    area = area,
    fictive_area = fictive_area,
    thermal_cluster = thermal_cluster,
    overwrite = overwrite,
    opts = opts
  )


# Get max hydro power that can be generated in a week
max_hydro <- get_max_hydro(area,opts)
res_cap <- get_reservoir_capacity(area,opts)
max_app <- max(readInputTS(hydroStorage = area , timeStep="weekly")$hydroStorage)

maxi <- min(max_hydro+max_app,res_cap)

# Generate Binding constraints of the flows capacities

constraint_values <- seq(from = 0, to = maxi, length.out = nb_disc_stock)
constraint_values <- round(constraint_values, 3)


#generate the flow sens constraints

if (match(area, sort(c(area, fictive_area))) == 1) {
  coeff_nn <- stats::setNames(-1, paste(area, fictive_area, sep = "%"))
} else {
  coeff_nn <- stats::setNames(1, paste(fictive_area, area, sep = "%"))
}


# Implement the flow sens in the study

antaresEditObject::createBindingConstraint(
  name = "nonnegative",
  enabled = TRUE,
  operator = "greater",
  coefficients = coeff_nn,
  opts = opts,
  overwrite = TRUE,
  timeStep = "hourly"
)

# Start the simulations

simulation_names <- vector(mode = "character", length = length(constraint_values))
for (i in constraint_values) {
  # Prepare simulation parameters
  name_bc <- paste0(binding_constraint, format(i, decimal.mark = ","))
  constraint_value <- round(i  / 7)
    # Coefficient
  if (match(area, sort(c(area, fictive_area))) == 1) {
    coeff <- stats::setNames(-1, paste(area, fictive_area, sep = "%"))
  } else {
    coeff <- stats::setNames(1, paste(fictive_area, area, sep = "%"))
  }
    # Implement binding constraint
  opts <- antaresEditObject::createBindingConstraint(
    name = name_bc,
    values = data.frame(less = rep(constraint_value, times = 366)),
    enabled = TRUE,
    timeStep = "weekly",
    operator = "less",
    overwrite = overwrite,
    coefficients = coeff,
    opts = opts
  )

  iii <- which(num_equal(i, constraint_values))
  message("#  ------------------------------------------------------------------------")
  message(paste0("Running simulation: ", iii, " - ", sprintf(simulation_name, format(i, decimal.mark = ","))))
  message("#  ------------------------------------------------------------------------")

  # run the simulation
  antaresEditObject::runSimulation(
    name = sprintf(simulation_name, format(i, decimal.mark = ",")),
    mode = "economy",
    wait = wait,
    path_solver = path_solver,
    show_output_on_console = show_output_on_console,
    opts = opts
  )
  simulation_names[which(constraint_values == i)] <- sprintf(simulation_name, format(i, decimal.mark = ","))

  #remove the Binding Constraints
  opts <- antaresEditObject::editBindingConstraint(name = name_bc, opts = opts,enabled = FALSE)

  #Simulation Control
  sim_name <-  sprintf(simulation_name, format(i, decimal.mark = ","))
  sim_name <- getSimulationNames(pattern =sim_name , opts = opts)[1]
  sim_check <- paste0(opts$studyPath,"/output")
  sim_check <- paste(sim_check,sim_name,sep="/")
  if(!dir.exists(paste0(sim_check,"/economy/mc-all"))) {
    stop("Simulation Error. Please check simulation log.")
  }
}

# remove the fictive area

removeArea(fictive_area,opts = opts)

# restore hydrostorage

restoreHydroStorage(area = area, opts = opts)

list(
  simulation_names = simulation_names,
  simulation_values = constraint_values
)

}
