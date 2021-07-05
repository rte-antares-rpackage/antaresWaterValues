#' Run Water Values Simulation and Calculate Value Node
#'
#' @param area The area concerned by the simulation.
#' @param nb_disc_stock Number of simulation to launch,
#' a vector of energy constraint will be created from 0 to
#' the hydro storage maximum and of length this parameter.
#' @param nb_cycle Number of times to run the algorithm.
#' @param nb_scenario Number of Monte-Carlo years to use in simulation, default is to use Antares' setting.
#' @param overwrite If area or cluster already exists, should they be overwritten?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a data.table
#' @export
#'
#' @importFrom antaresEditObject setSolverPath updateGeneralSettings
#' @importFrom antaresRead setSimulationPath
#'
#' @examples
#' \dontrun{
#'
#' waterValues(area = "fr")
#'
#' }
waterValues <- function(area, nb_disc_stock = 10, nb_cycle = 3, nb_scenario = NULL, overwrite = TRUE, opts = antaresRead::simOptions()) {

  opts <- try(opts, silent = TRUE)
  if ("try-error" %in% class(opts)) {
    message("Please select an Antares simulation directory")
    opts <- antaresRead::setSimulationPath(simulation = "input")
  }

  path_solver <- getOption("antares.solver")
  if (is.null(path_solver)) {
    message("Please select Antares' solver path")
    antaresEditObject::setSolverPath()
    path_solver <- getOption("antares.solver")
  }

  if (!is.null(nb_scenario)) {
    updateGeneralSettings(nbyears = nb_scenario)
  }

  simulation_res <- runWaterValuesSimulation(
    area = area,
    nb_disc_stock = nb_disc_stock,
    path_solver = path_solver,
    overwrite = overwrite,
    show_output_on_console = FALSE,
    opts = opts
  )

  suppressWarnings({
    opts <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  simulation_names <- simulation_res$simulation_names
  simulation_values <- simulation_res$simulation_values

  value_nodes <- meanGridLayer(
    area = area,
    simulation_names = simulation_names,
    simulation_values = simulation_values,
    nb_cycle = nb_cycle,
    na_rm = TRUE,
    opts = opts
  )

  return(value_nodes)
}
