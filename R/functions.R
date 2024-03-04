#' Restore the Pumping power series, used in \code{runWaterValuesSimulation}
#'
#' @param area A valid Antares area.
#' @param path_manual_backup Path to a manual backup.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param silent Boolean. True to run without messages.
#' @return An updated list containing various information about the simulation
restorePumpPower <- function(area, path_manual_backup = NULL, opts = antaresRead::simOptions(),silent=F) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  if (is.null(path_manual_backup)) {
    # Pump power ----
    path_pump_power_backup <- file.path(inputPath, "hydro", "common","capacity", paste0("backup_maxpower_",area,".txt"))

    if (file.exists(path_pump_power_backup)) {
      file.copy(
        from = path_pump_power_backup,
        to = file.path(inputPath, "hydro", "common","capacity", paste0("maxpower_",area,".txt")),
        overwrite = TRUE
      )
      unlink(x = path_pump_power_backup)
    } else {
      if(!silent) message("No backup found")
    }
  } else {
    file.copy(
      from = path_manual_backup,
      to = file.path(inputPath, "hydro", "common","capacity", paste0("maxpower_",area,".txt")),
      overwrite = TRUE
    )
  }

  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")

  invisible(res)
}

#' Reset to 0 the pumping power, used in \code{setupWaterValuesSimulation}
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
#' @return An updated list containing various information about the simulation.
resetPumpPower <- function(area, path_manual_storage = NULL, opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  # Pump power ----
  if (is.null(path_manual_storage)) {
    restorePumpPower(area,silent=T)
    path_pump_power <- file.path(inputPath, "hydro", "common","capacity",  paste0("maxpower_",area,".txt"))
  } else {
    path_pump_power <- path_manual_storage
  }

  if (file.exists(path_pump_power)) {

    # file's copy
    res_copy <- file.copy(
      from = path_pump_power,
      to = file.path(inputPath, "hydro", "common","capacity", paste0("backup_maxpower_",area,".txt")),
      overwrite = FALSE
    )
    if (!res_copy)
      stop("Impossible to backup pumping power file")

    # read pump power and initialize at 0
    pump_power <- utils::read.table(file = path_pump_power)
    pump_power[,3] <- 0
    utils::write.table(
      x = pump_power[, , drop = FALSE],
      file = path_pump_power,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t"
    )

  } else {

    message("No pumping power for this area, creating one")
    v <- rep(0, 365)
    h <- rep(24,365)
    utils::write.table(
      x = data.frame(v,h,v,h),
      file = path_pump_power,
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

#' Get reservoir capacity for concerned area, used in different functions
#'
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @export

get_reservoir_capacity <- function(area, opts=antaresRead::simOptions()){
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


#' Get max hydro power that can be generated in a week, used in different functions
#'
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param timeStep among "hourly", "daily" and "weekly"
#' @export

get_max_hydro <- function(area, opts=antaresRead::simOptions(),timeStep="hourly"){

  #import the table "standard credits" from "Local Data/ Daily Power and energy Credits"
  max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  if (utils::hasName(max_hydro, "hstorPMaxHigh")) {
    max_turb <- max_hydro[, max(max_hydro$hstorPMaxHigh)] * 168
  } else {
    if (timeStep=="hourly"){
      max_turb <- max_hydro$generatingMaxPower
      max_pump <- max_hydro$pumpingMaxPower
    } else if (timeStep=="daily"){
      max_hydro$day <- (max_hydro$timeId-1)%/%24+1
      max_hydro <- dplyr::summarise(dplyr::group_by(max_hydro,.data$day),turb=sum(.data$generatingMaxPower),
                             pump=sum(.data$pumpingMaxPower))
      max_turb <- max_hydro$turb
      max_pump <- max_hydro$pump
    } else if (timeStep=="weekly"){
      max_hydro$week <- (max_hydro$timeId-1)%/%168+1
      max_hydro <- dplyr::summarise(dplyr::group_by(max_hydro,.data$week),turb=sum(.data$generatingMaxPower),
                             pump=sum(.data$pumpingMaxPower))
      max_turb <- max_hydro$turb
      max_pump <- max_hydro$pump
    } else {message("timeStep not supported, change to hourly, weekly or daily")}

  }
  max_hydro <- data.frame(timeId=seq(nrow(max_hydro)))
  max_hydro$pump <- max_pump
  max_hydro$turb <- max_turb
  return(max_hydro)
}

#' Get inflow in a week, used in different functions
#'
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param mcyears Vector of years used to evaluate cost
#' @export

get_inflow <- function(area, opts=antaresRead::simOptions(),mcyears){

  suppressWarnings(inflow <- antaresRead::readInputTS(hydroStorage = area , timeStep="hourly"))
  if (nrow(inflow)==0){
    message("No inflow has been found, considering it as null")
    inflow <- data.table(expand.grid(timeId=1:52,tsId=mcyears,hydroStorage=0,area=area,time=NaN))
  } else {
    inflow$week <- (inflow$timeId-1)%/%168+1
    inflow <- dplyr::summarise(dplyr::group_by(inflow,.data$week,.data$tsId),
                               hydroStorage=sum(.data$hydroStorage)) %>%
      dplyr::rename("timeId"="week") %>%
      dplyr::mutate(area=area, time=NaN)
  }


  return(data.frame(inflow))
}

#' Get overall cost in a week, used in different functions
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param district The district concerned by the simulation.
#' @param mcyears Vector of years used to evaluate cost
#' @export

get_weekly_cost <- function(district, opts=antaresRead::simOptions(),mcyears){

  cost <- antaresRead::readAntares(districts = district, mcYears = mcyears,
                           timeStep = "hourly", opts = opts, select=c("OV. COST"))

  cost$week <- (cost$timeId-1)%/%168+1
  cost <- dplyr::summarise(dplyr::group_by(cost,.data$week,.data$mcYear),
                           ov_cost=sum(.data$`OV. COST`)) %>%
      dplyr::rename("timeId"="week")

  return(data.frame(cost))
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
#' @examples
#' \dontrun{
#' getSimulationNames("eco")
#' }
#' @export

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


#------------- to antares format -------
#' Convert water values to Antares format
#'
#' This function converts water values generated by \code{Grid_Matrix}
#' to the format expected by Antares: a 365*101 matrix, where
#' the rows are the 365 days of the year and the columns are round percentage values
#' ranging from 0 to 100 assessing the reservoir level.
#' Since \code{Grid_Matrix} output weekly values for an
#' arbitrary number of reservoir levels, interpolation is performed on both scales
#' in order to fit the desired format.
#'
#' @param data A data.table generated by \code{Grid_Matrix}
#' @param penalty_level_low Penalty for violating the bottom rule curve, comparable to the unsupplied energy cost
#' @param penalty_level_high Penalty for violating the top rule curve, comparable to the spilled energy cost
#' @param constant Boolean. Generate daily constant values by week. FALSE to do interpolation.
#' @param force_final_level Boolean. Whether final level should be constrained
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#'
#' @return A 365*101 numeric matrix
#' @export

to_Antares_Format <- function(data,penalty_level_low,penalty_level_high,constant=T,
                              force_final_level=F,penalty_final_level=0){

  data <- dplyr::group_by(data, .data$weeks) %>%
    dplyr::arrange(.data$states) %>%
    dplyr::mutate(vu_pen=dplyr::if_else(is.na(.data$vu_pen),dplyr::lead(.data$vu_pen),.data$vu_pen),
                  vu=dplyr::if_else(is.na(.data$vu),dplyr::lead(.data$vu),.data$vu))
  data <- as.data.table(data)

  # rescale levels to round percentages ranging from 0 to 100
  states_ref <- data[, .SD[1], by = c("statesid"), .SDcols = "states"]
  states_ref <- dplyr::mutate(states_ref, states_percent = 100*.data$states/max(.data$states))

  nearest_states <- states_ref$statesid[sapply(0:100, function(x) min(which(x-states_ref$states_percent<=0)))]

  states_ref_0_100 <- data.table(
    states_round_percent = 0:100,
    statesid = nearest_states
  )

  res <- CJ(weeks = unique(data$weeks), states_round_percent = 0:100)

  res <- res %>%
    dplyr::left_join(dplyr::select(states_ref_0_100,
                                   c("states_round_percent",
                                     "statesid")),by=c("states_round_percent"))

  max_state <- max(states_ref$states)

  res <- res %>% dplyr::left_join(dplyr::select(data,"weeks","statesid","vu_pen","level_low","level_high"),
                           by = c("weeks", "statesid")) %>%
    dplyr::mutate(level_low=.data$level_low/max_state*100,
                  level_high=.data$level_high/max_state*100)

  if (!force_final_level){
    res <- res %>%
      dplyr::mutate(vu=dplyr::case_when(.data$states_round_percent>.data$level_high ~ .data$vu_pen - penalty_level_high,
                                        .data$states_round_percent<.data$level_low ~ .data$vu_pen + penalty_level_low,
                                        TRUE ~ .data$vu_pen))
  } else {
    res <- res %>%
      dplyr::mutate(vu=dplyr::if_else(.data$weeks!=1,
                                      dplyr::case_when(.data$states_round_percent>.data$level_high ~ .data$vu_pen - penalty_level_high,
                                                                        .data$states_round_percent<.data$level_low ~ .data$vu_pen + penalty_level_low,
                                                                        TRUE ~ .data$vu_pen),
                                     dplyr::case_when(.data$states_round_percent>.data$level_high ~ .data$vu_pen - penalty_final_level,
                                                                      .data$states_round_percent<.data$level_low ~ .data$vu_pen + penalty_final_level,
                                                                        TRUE ~ .data$vu_pen)))
  }


  res <- res %>%
    dplyr::mutate(weeks=dplyr::if_else(.data$weeks>=2,.data$weeks-1,52))


  # reshape
  value_nodes_matrix <- dcast(
    data = res,
    formula = weeks ~ states_round_percent,
    value.var = "vu"
  )

  value_nodes_matrix$weeks <- NULL

  value_nodes_matrix_0 <- value_nodes_matrix[52,]

  if(!constant){
    if (!requireNamespace("zoo", quietly = TRUE)) {
      stop(
        "Package \"zoo\" must be installed to use this function.",
        call. = FALSE
      )
    }
    reshaped_matrix <- double(length = 0)
    last <- value_nodes_matrix[52,]
    for(i in 1:52){
      v <- unlist(value_nodes_matrix[i,])
      v[!is.finite(v)] <- NaN
      v <- sapply(v, function(x) c(rep(if (is.finite(x)) NA else NaN, 7), x))
      v[1,] <- unlist(last)
      tab <- apply(v,2,zoo::na.spline)
      tab <- tab[2:8,]
      reshaped_matrix <-rbind(reshaped_matrix,tab)
      last <-unlist(value_nodes_matrix[i,])
    }

  }else{
    reshaped_matrix <- value_nodes_matrix[rep(seq_len(nrow(value_nodes_matrix)), each = 7), ]
  }
  reshaped_matrix <- rbind(value_nodes_matrix_0,reshaped_matrix)

return(reshaped_matrix)
}

#' Add fictive production and fictive load to avoid infeasabilities with binding constraints
#'
#' @param area A valid Antares area.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.

add_fictive_fatal_prod_demand <- function(area, opts = antaresRead::simOptions()){

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  max_hydro <- get_max_hydro(area=area,opts=opts,timeStep="hourly") %>%
    dplyr::select(-c("timeId")) %>% max()

  # Input path
  inputPath <- opts$inputPath

  restore_fictive_fatal_prod_demand(area=area, opts = opts,silent=T)
  path_load <- file.path(inputPath, "load", "series", paste0("load_",area,".txt"))


  if (file.exists(path_load)) {

    # file's copy
    res_copy <- file.copy(
      from = path_load,
      to = file.path(inputPath, "load", "series", paste0("loadbackup_",area,".txt")),
      overwrite = FALSE
    )
    if (!res_copy)
      stop("Impossible to backup load file")

    load <- NULL
    try (load <- utils::read.table(file = path_load),silent = T)
    if (!is.null(load)){
      load <- load + max_hydro
      utils::write.table(
        x = load[,, drop = FALSE],
        file = path_load,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    } else {
      utils::write.table(
        x = data.frame(x = rep(max_hydro, 8760)),
        file = path_load,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    }

  } else {

    message("No load series for this area, creating one")

    utils::write.table(
      x = data.frame(x = rep(max_hydro, 8760)),
      file = path_load,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t"
    )

  }

  path_misc <- file.path(inputPath, "misc-gen", paste0("miscgen-",area,".txt"))


  if (file.exists(path_misc)) {

    # file's copy
    res_copy <- file.copy(
      from = path_misc,
      to = file.path(inputPath, "misc-gen", paste0("miscgen-backup-",area,".txt")),
      overwrite = FALSE
    )
    if (!res_copy)
      stop("Impossible to backup misc gen file")

    misc <- NULL
    try (misc <- utils::read.table(file = path_misc),silent = T)
    if (!is.null(misc)){
      misc[,6] <- misc[,6] + max_hydro
      utils::write.table(
        x = misc[,, drop = FALSE],
        file = path_misc,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    } else {
      utils::write.table(
        x = data.frame(x = matrix(c(rep(0, 8760*5),
                                    rep(max_hydro, 8760),
                                    rep(0, 8760*2)), ncol = 8)),
        file = path_misc,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    }

  } else {

    message("No misc series for this area, creating one")

    utils::write.table(
      x = data.frame(x = matrix(c(rep(0, 8760*5),
                                  rep(max_hydro, 8760),
                                  rep(0, 8760*2)), ncol = 8)),
      file = path_misc,
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

#' Restore load and misc gen time series
#'
#' @param area A valid Antares area.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param silent Boolean. True to run without messages.
#' @return An updated list containing various information about the simulation.
#'
restore_fictive_fatal_prod_demand <- function(area, opts = antaresRead::simOptions(),silent=F) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  path_load <- file.path(inputPath, "load", "series", paste0("loadbackup_",area,".txt"))

  if (file.exists(path_load)) {
    file.copy(
      from = path_load,
      to = file.path(inputPath, "load", "series", paste0("load_",area,".txt")),
      overwrite = TRUE
    )
    unlink(x = path_load)
  } else {
    if(!silent) message("No load backup found")
  }

  path_misc <- file.path(inputPath, "misc-gen", paste0("miscgen-backup-",area,".txt"))

  if (file.exists(path_misc)) {
    file.copy(
      from = path_misc,
      to = file.path(inputPath, "misc-gen", paste0("miscgen-",area,".txt")),
      overwrite = TRUE
    )
    unlink(x = path_misc)
  } else {
    if(!silent) message("No misc gen backup found")
  }

  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")

  invisible(res)
}
