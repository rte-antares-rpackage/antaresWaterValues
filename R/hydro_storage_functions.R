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





#' Reset to 0 the hydro storage time series
#'
#'
#' @param area A valid Antares area.
#' @param path Optional, a path where to save the hydro storage file.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @note The function makes a copy of the original hydro storage time series,
#'  you can restore these with \code{restoreHydroStorage}.
#'
#' @seealso \link{restoreHydroStorage}
#'
#' @importFrom utils read.table write.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
# @examples
resetHydroStorage <- function(area, path = NULL, opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath






  # Hydro storage ----
  if (is.null(path)) {
    path_test <-  file.path(inputPath, "hydro", "series", area, "mod_backup.txt")

      #In case there is mod_backup from an interrupted simulation
    if (file.exists(path_test)) {
      file.copy(
        from = path_test,
        to = file.path(inputPath, "hydro", "series", area, "mod.txt"),
        overwrite = TRUE
      )
      unlink(x=path_test)
    }


    path_hydro_storage <- file.path(inputPath, "hydro", "series", area, "mod.txt")
  } else {
    path_hydro_storage <- path
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
    hydro_storage <- utils::read.table(file = path_hydro_storage)
    hydro_storage[] <- 0
    utils::write.table(
      x = hydro_storage[, 1, drop = FALSE],
      file = path_hydro_storage,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t"
    )

  } else {

    message("No hydro storage series for this area, creating one")

    utils::write.table(
      x = data.frame(x = rep(0, 12)),
      file = path_hydro_storage,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t"
    )

  }

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })

  invisible(res)
}


#' Get the reservoir capacity for an area
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


#' Calculate  the hydro cost
#'
#' @param area A valid Antares area.
#' @param simulation_name simulation name in output folder.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return A real. the cost of hydro: stock Diff cost and hydro cost.
#'
#' @importFrom antaresRead readAntares setSimulationPath
#' @export
#'
#'

hydro_cost <- function(area,mcyears,simulation_name,opts){

  tmp_opt <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = simulation_name)

  data <-antaresRead::readAntares(area = area, timeStep = "monthly",mcYears = mcyears,
                     opts = opts, showProgress = F)

  if(length(data)==0)
    {
    data <- antaresRead::readAntares(area = area, timeStep = "weekly" ,
                mcYears = mcyears, opts = opts,showProgress = F)
   }

  hydro_stockDiff <- data$`H. LEV`[nrow(data)]-data$`H. LEV`[1]
  res_cap <- get_reservoir_capacity(area,opts)
  hydro_stockDiff <- hydro_stockDiff*res_cap
  hydro_stockDiff_cost <- hydro_stockDiff*mean(data$`MRG. PRICE`)
  hydro_cost <- antaresRead::readAntares(area = area, timeStep = "annual" ,
                            mcYears = mcyears, opts = opts,showProgress = F,
                            select = "H. COST")$`H. COST`
  total_hydro_cost <- hydro_cost+hydro_stockDiff_cost
  return(total_hydro_cost)
}
