#' Restore the hydro storage time series, used in \code{runWaterValuesSimulation}
#'
#' @param area A valid Antares area.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param data Backup hydro storage matrix
#' @return An updated list containing various information about the simulation.
#'
restoreHydroStorage <- function(area, opts = antaresRead::simOptions(),data) {
  antaresEditObject::writeInputTS(data,
                                 area=area,
                                 type="hydroSTOR",
                                 opts=opts)
  hydro_ini <- antaresRead::readIni(file.path("input","hydro","hydro"),opts=opts)
  hydro_ini$reservoir[area]=T
  antaresEditObject::writeIni(hydro_ini,
                              file.path("input","hydro","hydro"),
                              overwrite=T,
                              opts=opts)
}

#' Reset to 0 the hydro storage time series, used in \code{setupWaterValuesSimulation}
#'
#'
#' @param area A valid Antares area.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @note You can restore the original hydro storage time series with \code{restoreHydroStorage}.
#'
#' @seealso \link{restoreHydroStorage}
#'
#'
#' @return An updated list containing various information about the simulation.
#'
resetHydroStorage <- function(area, opts = antaresRead::simOptions()) {
  antaresEditObject::writeInputTS(matrix(rep(0,365),ncol=1),
                                  area=area,
                                  type="hydroSTOR",
                                  opts=opts)
  hydro_ini <- antaresRead::readIni(file.path("input","hydro","hydro"),opts=opts)
  hydro_ini$reservoir[area]=F
  antaresEditObject::writeIni(hydro_ini,
                              file.path("input","hydro","hydro"),
                              overwrite=T,
                              opts=opts)
}

#' Get the Pumping efficiency ratio for an area reservoir
#'
#' @param area An 'antares' area.
#' @param force If "reservoir management" is disabled, return anyway the reservoir capacity?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return the reservoir capacity (in MWh), or \code{NULL} if none.
#' @export
#'
getPumpEfficiency <- function(area, force = FALSE, opts = antaresRead::simOptions()) {
  area = tolower(area)
  hydro_ini <- antaresRead::readIni(file.path("input","hydro","hydro"),opts=opts)
  if (isTRUE(hydro_ini$reservoir[[area]]) | force) {
    Pump_Efficiency <- hydro_ini[["pumping efficiency"]][[area]]
  } else {
    Pump_Efficiency <- NULL
  }
  Pump_Efficiency
}

#' Change the mode of management of an area
#'
#' @param watervalues Binary. T if use watervalues
#' @param heuristic Binary. T if use heuristic
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param area Antares area
#' @export
changeHydroManagement <- function(watervalues=F,heuristic=T,opts,area){
  hydro_ini <- antaresRead::readIni(file.path("input","hydro","hydro"),opts=opts)
  assertthat::assert_that(area %in% names(hydro_ini$reservoir),msg = "No reservoir managment for this area, check Antares study")
  assertthat::assert_that(hydro_ini$reservoir[area]==T,msg="No reservoir managment for this area, check Antares study")
  assertthat::assert_that((watervalues|heuristic)==T,msg="Watervalues or heuristic has to be selected")

  if (watervalues){
    hydro_ini[["use water"]][[area]] <- TRUE
    hydro_ini[["use heuristic"]][[area]] <- heuristic
  } else {
    if (area %in% names(hydro_ini[["use water"]])){
      hydro_ini[["use water"]][[area]] <- FALSE
    }
    if (area %in% names(hydro_ini[["use heuristic"]])){
      hydro_ini[["use heuristic"]][[area]] <- TRUE
    }
  }
  antaresEditObject::writeIni(hydro_ini,
                              file.path("input","hydro","hydro"),
                              overwrite=T,
                              opts=opts)
}

