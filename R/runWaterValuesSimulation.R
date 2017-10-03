#' Run a simulation for calculating water values for a specific area
#'
#' @param area The area concerned by the simulation.
#' @param simulation_name The name of the simulation, \code{s} is a placeholder for the constraint value defined by \code{nb_simulation}.
#' @param nb_simulation Number of simulation to launch, a vector of energy constraint
#'  will be created from 0 to the hydro storage maximum and of length this parameter.
#' @param binding_constraint Name of the binding constraint. 
# @param constraint_values Vector of energy constraints on the link between the area and the fictive area.
#' @param fictive_area Name of the fictive area to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param thermal_cluster Name of the thermal cluster to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param path_solver Character containing the Antares Solver path, argument passed to \code{\link[antaresXpansion]{run_simulation}}.
#' @param wait Argument passed to \code{\link[antaresXpansion]{run_simulation}}.
#' @param show_output_on_console Argument passed to \code{\link[antaresXpansion]{run_simulation}}.
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
#' @importFrom antaresEditObject createBindingConstraint removeBindingConstraint readIniFile writeIni
#' @importFrom antaresRead readClusterDesc
#' @importFrom antaresXpansion run_simulation
#' @importFrom stats setNames
#'
# @examples
runWaterValuesSimulation <- function(area,
                                     simulation_name = "weekly_water_amount_%s",
                                     nb_simulation = 10,
                                     binding_constraint = "WeeklyWaterAmount", 
                                     # constraint_values,
                                     fictive_area = NULL,
                                     thermal_cluster = NULL,
                                     path_solver,
                                     wait = TRUE,
                                     show_output_on_console = TRUE,
                                     overwrite = FALSE, 
                                     opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  
  fictive_area <- if (!is.null(fictive_area)) fictive_area else paste0("watervalue_", area)
  thermal_cluster <- if (!is.null(thermal_cluster)) thermal_cluster else "WaterValueCluster"
  
  # setup study
  opts <- setupWaterValuesSimulation(
    area = area,
    fictive_area = fictive_area, 
    thermal_cluster = thermal_cluster,
    overwrite = overwrite, 
    opts = opts
  )
  
  # Get reservoir capacity for concerned area
  hydro_ini <- antaresEditObject::readIniFile(file.path(opts$inputPath, "hydro", "hydro.ini"))
  if (isTRUE(hydro_ini$reservoir[[area]])) {
    reservoir_capacity <- hydro_ini[["reservoir capacity"]][[area]]
    if (is.null(reservoir_capacity))
      stop(paste0("Incorrect reservoir capacity for area: ", area))
    reservoir_capacity <- reservoir_capacity / 10
  } else {
    reservoir_capacity <- 1
  }
  
  
  # Get hydro max power
  # max_hydro <- antaresRead::readClusterDesc()[area==watervalue_fr, c(nominalcapacity)]
  max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  max_hydro <- max_hydro[, max(hstorPMaxHigh)]*168/1e6
  constraint_values <- seq(from = 0, to = max_hydro, length.out = nb_simulation)
  

  simulation_names <- vector(mode = "character", length = length(constraint_values))
  for (i in constraint_values) {
    name_bc <- paste0(binding_constraint, format(i, decimal.mark = ","))
    constraint_value <- i * reservoir_capacity / 7  # stock max lac pays (Fr+ch)           ####################
    # Create binding constraint
    opts <- antaresEditObject::createBindingConstraint(
      name = name_bc, 
      values = data.frame(less = rep(constraint_value, times = 366)),
      enabled = TRUE,
      timeStep = "weekly", 
      operator = "less",
      overwrite = overwrite, 
      coefficients = stats::setNames(-1, paste(area, fictive_area, sep = "%")), ####
      opts = opts
    )
    message("#  ------------------------------------------------------------------------")
    message(paste0("Running simulation: ", sprintf(simulation_name, format(i, decimal.mark = ","))))
    message("#  ------------------------------------------------------------------------")
    antaresXpansion::run_simulation(
      name = sprintf(simulation_name, format(i, decimal.mark = ",")), 
      mode = "economy",
      wait = wait,
      path_solver = path_solver, 
      show_output_on_console = show_output_on_console,
      opts = opts
    )
    path_output <- list.files(
      path = file.path(opts$studyPath, "output"), 
      pattern =  paste0(sprintf(simulation_name, format(i, decimal.mark = ",")), "$")
    )
    path_output <- file.path(file.path(opts$studyPath, "output"), path_output, "watervalues.ini")
    antaresEditObject::writeIni(listData = list(general = list(watervalue = i)), pathIni = path_output, overwrite = TRUE)
    simulation_names[which(constraint_values == i)] <- sprintf(simulation_name, format(i, decimal.mark = ","))
    opts <- antaresEditObject::removeBindingConstraint(name = name_bc, opts = opts)
  }
  
  # restore hydrostorage
  restoreHydroStorage(area = area, opts = opts)
  
  list(
    simulation_names = simulation_names,
    simulation_values = constraint_values
  )
}
