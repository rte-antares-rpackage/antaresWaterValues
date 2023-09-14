#' Restore the hydro storage time series, used in \code{runWaterValuesSimulation}
#'
#' @param area A valid Antares area.
#' @param path_manual_backup Path to a manual backup.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param silent Boolean. True to run without messages.
#' @return An updated list containing various information about the simulation.
#'
restoreHydroStorage <- function(area, path_manual_backup = NULL, opts = antaresRead::simOptions(),silent=F) {

  if(antaresEditObject:::is_api_study(opts)){
    message("API study : Restoration is deactivated")
    return(NULL)
  }

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  if (is.null(path_manual_backup)) {
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
      if(!silent) message("No backup found")
    }
  } else {
    file.copy(
      from = path_manual_backup,
      to = file.path(inputPath, "hydro", "series", area, "mod.txt"),
      overwrite = TRUE
    )
  }

  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")

  invisible(res)
}

#' Reset to 0 the hydro storage time series, used in \code{setupWaterValuesSimulation}
#'
#'
#' @param area A valid Antares area.
#' @param path_manual_storage Optional, a path where to save the hydro storage file.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @note The function makes a copy of the original hydro storage time series,
#'  you can restore these with \code{restoreHydroStorage}.
#'
#' @seealso \link{restoreHydroStorage}
#'
#'
#' @return An updated list containing various information about the simulation.
#'
resetHydroStorage <- function(area, path_manual_storage = NULL, opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  # Hydro storage ----
  if (is.null(path_manual_storage)) {
    restoreHydroStorage(area,silent=T)
    path_hydro_storage <- file.path(inputPath, "hydro", "series", area, "mod.txt")
  } else {
    path_hydro_storage <- path_manual_storage
  }

  if (file.exists(path_hydro_storage)) {

    # file's copy
    res_copy <- file.copy(
      from = path_hydro_storage,
      to = file.path(inputPath, "hydro", "series", area, "mod_backup.txt"),
      overwrite = FALSE
    )
    if (!res_copy)
      stop("Impossible to backup hydro storage file")

    # read hydro storage series and initialize at 0
    hydro_storage <- NULL
    try (hydro_storage <- antaresRead:::fread_antares(file = path_hydro_storage,
                                                      opts = opts),
         silent = T)
    if (!is.null(hydro_storage)){
      hydro_storage[] <- 0
      antaresEditObject::writeInputTS(hydro_storage[,, drop = FALSE],
                                      area=area,
                                      type="hydroSTOR")
    }

  } else {

    message("No hydro storage series for this area, creating one")

    antaresEditObject::writeInputTS(data.frame(x = rep(0, 12)),
                                    area=area,
                                    type="hydroSTOR")

  }

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })

  invisible(res)
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
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% antaresRead::getAreas(opts = opts))
    stop("Not a valid area!")
  hydro_ini <- antaresEditObject::readIni(file.path("input","hydro","hydro"),opts=opts)
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
changeHydroManagement <- function(watervalues=F,heuristic=T,opts,area){
  hydro_ini <- antaresEditObject::readIni(file.path("input","hydro","hydro"),opts=opts)
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

