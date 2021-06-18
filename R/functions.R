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







#' Restore the hydro storage time series
#'
#' @param area A valid Antares area.
#' @param path Path to a manual backup.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath
#'
# @examples
restoreHydroStorage <- function(area, path = NULL, opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  if (is.null(path)) {
    # Hydro storage ----
    path_hydro_storage_backup <- file.path(inputPath, "hydro", "series", area, "mod_backup.txt")

    if (file.exists(path_hydro_storage_backup)) {
      file.copy(
        from = path_hydro_storage_backup,
        to = file.path(inputPath, "hydro", "series", area, "mod.txt"),
        overwrite = TRUE
      )
      unlink(x = path_hydro_storage_backup)
    } else {
      message("No backup found")
    }
  } else {
    file.copy(
      from = path,
      to = file.path(inputPath, "hydro", "series", area, "mod.txt"),
      overwrite = TRUE
    )
  }

  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")

  invisible(res)
}





