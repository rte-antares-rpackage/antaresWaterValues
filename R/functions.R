#' Get reservoir capacity for concerned area
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
get_reservoir_capacity <- function(area, opts=antaresRead::simOptions())

{
  hydro_ini <- antaresEditObject::readIniFile(file.path(opts$inputPath, "hydro", "hydro.ini"))
if (isTRUE(hydro_ini$reservoir[[area]])) {
  reservoir_capacity <- hydro_ini[["reservoir capacity"]][[area]]
  if (is.null(reservoir_capacity))
    reservoir_capacity <- getOption("watervalues.reservoir_capacity", default = 1e7)
  reservoir_capacity <- reservoir_capacity
} else {
  reservoir_capacity <- 1
}
return(reservoir_capacity)
  }


#' Get max hydro power that can be generated in a week
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'

get_max_hydro <- function(area, opts=antaresRead::simOptions())
{
#import the table "standard credits" from "Local Data/ Daily Power and energy Credits"
max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
if (hasName(max_hydro, "hstorPMaxHigh")) {
  max_hydro <- max_hydro[, max(hstorPMaxHigh)] * 168
} else {
  max_hydro <- max_hydro[, max(generatingMaxPower)] * 168    }
return(max_hydro)
}




#' Utility function to get simulation's name
#'
#' @param pattern A pattern to match among the simulation.
#' @param studyPath Path to study outputs, used if \code{setSimulationPath} is not set.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' getSimulationNames("eco")
#' }
getSimulationNames <- function(pattern, studyPath = NULL, opts = antaresRead::simOptions()) {
  studyPath <- tryCatch({
    opts$studyPath
  }, error = function(e) {
    studyPath
  })
  if (is.null(studyPath))
    stop("Default antares options are not set, you must specify 'studyPath'.")
  list.files(path = file.path(studyPath, "output"), pattern = pattern)
}


#------------- get turbaned capacity from reward table-----

names_reward <-function(reward_dt){
  j <- 3
  names <- names(reward_dt)
  if (names[1]=="weekly_water_amount_0")
    {j <- 1}
  values <- gsub("weekly_water_amount_","",names[3:length(names)])
  values <- as.numeric(sub(",", ".", values, fixed = TRUE))
  values <- as.integer(values)
  return(values)
}

