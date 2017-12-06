#' Run Water Values Simulation and Calculate Value Node
#'
#' @param area The area concerned by the simulation.
#' @param nb_simulation Number of simulation to launch, 
#' a vector of energy constraint will be created from 0 to 
#' the hydro storage maximum and of length this parameter.
#' @param n_runs Number of times to run the algorithm.
#'
#' @return a data.table
#' @export
#' 
#' @importFrom antaresEditObject setSolverPath
#'
#' @examples
#' \dontrun{
#' 
#' waterValues(area = "fr")
#' 
#' }
waterValues <- function(area, nb_simulation = 10, n_runs = 3) {
  
  path_solver <- getOption("antares.solver")
  if (is.null(path_solver)) {
    antaresEditObject::setSolverPath()
    path_solver <- getOption("antares.solver")
  }
  
  simulation_res <- runWaterValuesSimulation(
    area = area,
    nb_simulation = nb_simulation,
    path_solver = path_solver, 
    overwrite = TRUE
  )
  
  simulation_names <- simulation_res$simulation_names
  simulation_values <- simulation_res$simulation_values
  
  value_nodes <- meanGridLayer(
    area = area,
    simulation_names = simulation_names, 
    simulation_values = simulation_values,
    n_runs = n_runs
  )
  
  return(value_nodes)
}




