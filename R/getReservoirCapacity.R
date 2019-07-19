#' Get the reservoir capacity for an area
#'
#' @param area An 'antares' area.
#' @param force If "reservoir management" is disabled, return anyway the reservoir capacity?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @return the reservoir capacity (in TWh), or \code{NULL} if none.
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead getAreas
#' @importFrom antaresEditObject readIniFile
#'
#' @examples
#' \dontrun{
#' 
#' getReservoirCapacity("fr")
#' 
#' }
getReservoirCapacity <- function(area, force = FALSE, opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% antaresRead::getAreas(opts = opts)) 
    stop("Not a valid area!")
  hydro_ini <- antaresEditObject::readIniFile(file.path(opts$inputPath, "hydro", "hydro.ini"))
  if (isTRUE(hydro_ini$reservoir[[area]]) | force) {
    reservoir_capacity <- hydro_ini[["reservoir capacity"]][[area]]
  } else {
    reservoir_capacity <- NULL
  }
  reservoir_capacity
}
