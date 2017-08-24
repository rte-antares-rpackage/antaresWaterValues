#' Utility function to get simulation's name
#'
#' @param pattern A pattern to match among the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' getSimulationNames("eco")
getSimulationNames <- function(pattern, opts = antaresRead::simOptions()) {
  list.files(path = file.path(opts$studyPath, "output"), pattern = pattern)
}

