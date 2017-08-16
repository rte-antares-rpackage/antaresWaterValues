#' Run a simulation for calculating water values for a specific area
#'
#' @param area The area concerned by the simulation.
#' @param simulation_name The name of the simulation, \code{s} is a placeholder for the constraint value,
#'  this is usefull if \code{constraint_values} is a vector.
#' @param binding_constraint Name of the binding constraint. 
#' @param constraint_values Vector of energy constraints on the link between the area and the fictive area.
#' @param fictive_area Name of the fictive area to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param thermal_cluster Name of the thermal cluster to create, argument passed to \code{\link{setupWaterValuesSimulation}}.
#' @param path_solver Character containing the Antares Solver path, argument passed to \code{\link[antaresXpansion]{run_simulation}}.
#' @param wait Argument passed to \code{\link[antaresXpansion]{run_simulation}}.
#' @param show_output_on_console Argument passed to \code{\link[antaresXpansion]{run_simulation}}.
#' @param overwrite If area or cluster already exists, overwrite them ?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresEditObject createBindingConstraint removeBindingConstraint
#' @importFrom antaresXpansion run_simulation
#'
# @examples
runWaterValuesSimulation <- function(area,
                                     simulation_name = "WeeklyWaterAmount%s",
                                     binding_constraint = "WeeklyWaterAmount", 
                                     constraint_values,
                                     fictive_area = NULL,
                                     thermal_cluster = NULL,
                                     path_solver,
                                     wait = TRUE,
                                     show_output_on_console = TRUE,
                                     overwrite = FALSE, 
                                     opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  
  # setup study
  setupWaterValuesSimulation(
    area = area,
    fictive_area = if (!is.null(fictive_area)) fictive_area else paste0("WaterValue_", area), 
    thermal_cluster = if (!is.null(thermal_cluster)) thermal_cluster else "WaterValueCluster",
    overwrite = overwrite
  )
  
  
  for (i in constraint_values) {
    name_bc <- paste0(binding_constraint, format(i, decimal.mark = ","))
    # Create binding constraint
    antaresEditObject::createBindingConstraint(
      name = name_bc, 
      values = data.frame(less = rep(constraint_values, times = 366)),
      timeStep = "weekly", 
      operator = "less",
      overwrite = overwrite
    )
    antaresXpansion::run_simulation(
      name = sprintf(simulation_name, format(i, decimal.mark = ",")), 
      mode = "economy",
      wait = wait,
      path_solver = path_solver, 
      show_output_on_console = show_output_on_console
    )
    antaresEditObject::removeBindingConstraint(name = name_bc, opts = opts)
  }
  
  invisible()
}