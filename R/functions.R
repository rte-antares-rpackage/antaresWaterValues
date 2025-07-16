#' Restore the Pumping power series, used in \code{runWaterValuesSimulation}
#'
#' @param area A valid Antares area.
#' @param path_manual_backup Path to a manual backup.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param silent Boolean. True to run without messages.
#' @return An updated list containing various information about the simulation
#' @keywords internal
restorePumpPower <- function(area, path_manual_backup = NULL, opts = antaresRead::simOptions(),silent=F) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  if (is.null(path_manual_backup)) {
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
#' @keywords internal
resetPumpPower <- function(area, path_manual_storage = NULL, opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

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

#' Get reservoir capacity
#'
#' Get hydro reservoir capacity for the given area. Reservoir management must be on.
#'
#' @inheritParams runWaterValuesSimulation
#'
#' @returns Double, reservoir capacity in MWh.
#'
#' @export
get_reservoir_capacity <- function(area, opts){
  hydro_ini <- antaresRead::readIniFile(file.path(opts$inputPath, "hydro", "hydro.ini"))
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


#' Get pumping and generating hydro power
#'
#' Get maximum pumping and generating hydro capacities over a given horizon given in \code{timeStep}.
#'
#' @inheritParams runWaterValuesSimulation
#' @param timeStep Character among "hourly", "daily" and "weekly".
#'
#' @returns A \code{dplyr::tibble()} with 3 columns : \code{"timeId"}, \code{"pump"} and \code{"turb"}.
#'
#' @export
get_max_hydro <- function(area, opts,timeStep="hourly"){

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

#' Get weekly inflows
#'
#' Get weekly inflows of the given area for all weeks from 1 to 52 and for all time-series in \code{mcyears}.
#' Please note that if time series index don't correspond to scenario number in scenario builder, this could generate inconsistencies.
#'
#' @inheritParams runWaterValuesSimulation
#'
#' @returns A \code{dplyr::tibble()} with 4 columns : \code{"timeId"} (integer), \code{"tsId"} (integer), \code{"hydroStorage"} (double) and \code{"area"} (character).
#' Each line give the amount of weekly inflow (\code{"hydroStorage"}) for \code{area} for a week (\code{"timeId"}) and a scenario (\code{"tsId"}).
#'
#' @export
get_inflow <- function(area, opts,mcyears){

  suppressWarnings(inflow <- antaresRead::readInputTS(hydroStorage = area , timeStep="hourly"))
  if (nrow(inflow)==0){
    message("No inflow has been found, considering it as null")
    inflow <- data.table(expand.grid(timeId=1:52,tsId=mcyears,hydroStorage=0,area=area))
  } else {
    inflow$week <- (inflow$timeId-1)%/%168+1
    inflow <- dplyr::summarise(dplyr::group_by(inflow,.data$week,.data$tsId),
                               hydroStorage=sum(.data$hydroStorage)) %>%
      dplyr::rename("timeId"="week") %>%
      dplyr::mutate(area=area)
    if (length(unique(inflow$tsId))==1){
      inflow <- inflow %>%
        dplyr::select(-("tsId")) %>%
        dplyr::cross_join(data.table(tsId=mcyears))
    }
    assertthat::assert_that(all(mcyears %in% unique(inflow$tsId)),msg = "Couldn't find inflow for all scenarios, please modify your study.")
  }


  return(data.table(inflow))
}

#' Get objective values
#'
#' Get objective values of the optimization problem of each week and each scenario for a given simulation \code{simu}, mainly used in \code{get_Reward()} to build reward functions.
#'
#' @inheritParams get_local_reward
#' @inheritParams runWaterValuesSimulation
#' @param district The district concerned by the simulation.
#' @param fictive_areas Vector of chr. Fictive areas used in simulation.
#'
#' @return A \code{dplyr::tibble()} with 3 columns : \code{"timeId"}, \code{"mcYear"} and \code{"ov_cost"}.
#'
#' @export
get_weekly_cost <- function(district, simu,mcyears,expansion=F,fictive_areas=NULL){

  criterium_file <- FALSE
  if (expansion){
    path <- paste0(simu$simPath)
    all_files <- list.files(path)
    criterium_file <- sum(stringr::str_detect(all_files,"criterion")) >= 52*length(mcyears)
  }
  if (!criterium_file){
    cost <- antaresRead::readAntares(districts = district, mcYears = mcyears,
                                     timeStep = "hourly", opts = simu, select=c("OV. COST"))
    cost$week <- (cost$timeId-1)%/%168+1
    cost <- dplyr::summarise(dplyr::group_by(cost,.data$week,.data$mcYear),
                             ov_cost=sum(.data$`OV. COST`)) %>%
      dplyr::rename("timeId"="week")
  } else {
    if (is.null(fictive_areas)){
      cost <- data.frame(tidyr::expand_grid(mcYear=mcyears,timeId=1:52))%>%
        dplyr::mutate(ov_cost=0,cost_xpansion=0)
    } else {
      cost <- antaresRead::readAntares(areas = fictive_areas, mcYears = mcyears,
                                       timeStep = "hourly", opts = simu, select=c("OV. COST"))
      cost$week <- (cost$timeId-1)%/%168+1
      cost <- dplyr::summarise(dplyr::group_by(cost,.data$week,.data$mcYear),
                               ov_cost=sum(.data$`OV. COST`)) %>%
        dplyr::rename("timeId"="week") %>%
        dplyr::mutate(cost_xpansion=0)
    }

    for (week in 1:52){
      for (scenario in mcyears){
        path <- all_files[stringr::str_detect(all_files,paste0("criterion-",
                                                               scenario,"-",
                                                               week,"-"))][[1]]
        cost_xpansion <- as.numeric(strsplit(utils::read.delim(paste0(simu$simPath,"/",path),
                                                        header = FALSE)[[1]],":")[[1]][[2]])
        cost[(cost$timeId==week)&(cost$mcYear==scenario),4] <- cost_xpansion
      }
    }
    cost <- cost %>%
      dplyr::mutate(ov_cost =.data$cost_xpansion-.data$ov_cost) %>%
      dplyr::select(-c("cost_xpansion"))
  }

  return(data.table(cost))
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
#' @keywords internal
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


#' Convert water values to Antares format
#'
#' This function converts water values generated by \code{Grid_Matrix()}
#' to the format expected by Antares: a 365*101 matrix, where
#' the rows are the 365 days of the year and the columns are round percentage values
#' ranging from 0 to 100 assessing the reservoir level.
#' Use \code{antaresEditObject::writeWaterValues()} to write water values.
#'
#' @param data A \code{dplyr::tibble()} representing water values, output \code{aggregated_results} generated by \code{Grid_Matrix()}.
#' @param constant Binary. Generate daily constant values by week. \code{FALSE} to do interpolation.
#' @seealso \code{to_Antares_Format_bis()}
#' @note Please use \code{to_Antares_Format_bis()} if you are using \code{hydro-pricing-mode=accurate} in Antares.
#' @returns A 365*101 numeric matrix
#' @export
to_Antares_Format <- function(data,constant=T){

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

  res <- res %>% dplyr::left_join(dplyr::select(data,"weeks","statesid","vu_pen",
                                                "level_low","level_high",
                                                "penalty_high","penalty_low"),
                           by = c("weeks", "statesid")) %>%
    dplyr::mutate(level_low=.data$level_low/max_state*100,
                  level_high=.data$level_high/max_state*100)

  res <- res %>%
    dplyr::mutate(vu=dplyr::case_when(.data$states_round_percent>.data$level_high ~ .data$vu_pen - .data$penalty_high,
                                      .data$states_round_percent<.data$level_low ~ .data$vu_pen + .data$penalty_low,
                                      TRUE ~ .data$vu_pen))


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

#' Convert water values to Antares format with high accuracy
#'
#' @inheritParams to_Antares_Format
#' @inherit to_Antares_Format description
#' @inherit to_Antares_Format return
#' @seealso \code{to_Antares_Format()}
#' @export
to_Antares_Format_bis <- function(data){

  vb <- convert_to_percent(data)

  for (i in 1:nrow(vb)){
    vb[i,4] <- ifelse(vb[i,2]!=0,2*vb[i,3]-vb[i-1,4],0)
  }

  res <- vb %>%
    dplyr::ungroup() %>%
    as.data.table()


  # reshape
  value_nodes_matrix <- dcast(
    data = res,
    formula = weeks ~ states_round_percent,
    value.var = "vu"
  )

  value_nodes_matrix$weeks <- NULL

  value_nodes_matrix_0 <- value_nodes_matrix[52,]

  reshaped_matrix <- value_nodes_matrix[rep(seq_len(nrow(value_nodes_matrix)), each = 7), ]

  reshaped_matrix <- rbind(value_nodes_matrix_0,reshaped_matrix)

  return(reshaped_matrix)
}

convert_to_percent <- function(data){
  capa <- max(data$states)
  res <- tidyr::expand_grid(
    states_round_percent = 0:100, weeks = unique(data$weeks)
  )

  res <- res %>%
    dplyr::mutate(state_ref = .data$states_round_percent * capa/100) %>%
    dplyr::left_join(data,by=c("weeks")) %>%
    dplyr::select(c("weeks","states","value_node","state_ref",
                    "states_round_percent","level_low","level_high",
                    "penalty_low","penalty_high"))

  top <- res %>%
    dplyr::filter(.data$states >= .data$state_ref) %>%
    dplyr::group_by(.data$weeks,.data$state_ref) %>%
    dplyr::filter(.data$states==min(.data$states)) %>%
    dplyr::select(c("weeks","state_ref","states","value_node",
                    "states_round_percent","level_low","level_high",
                    "penalty_low","penalty_high"))

  bottom <- res %>%
    dplyr::filter(.data$states <= .data$state_ref) %>%
    dplyr::group_by(.data$weeks,.data$state_ref) %>%
    dplyr::filter(.data$states==max(.data$states)) %>%
    dplyr::select(c("weeks","state_ref","states","value_node","states_round_percent"))

  vb <- dplyr::left_join(bottom,top,by=c("weeks","state_ref","states_round_percent"),
                         suffix=c("_min","_max")) %>%
    dplyr::mutate(vb = (.data$value_node_max-.data$value_node_min)/(
      .data$states_max-.data$states_min)*(.data$state_ref-.data$states_min)+.data$value_node_min,
      vb = dplyr::if_else(.data$states_min==.data$states_max,.data$value_node_min,.data$vb)) %>%
    dplyr::arrange(.data$weeks, .data$state_ref) %>%
    dplyr::group_by(.data$weeks) %>%
    dplyr::mutate(value_node_dif=.data$vb-dplyr::lag(.data$vb),# Delta of Bellman values
                  states_dif=.data$state_ref-dplyr::lag(.data$state_ref), # Delta of states
                  mu=.data$value_node_dif /.data$states_dif, # Ratio
                  mu=round(.data$mu,2),
                  vu=0) %>%
    dplyr::mutate(level_low=.data$level_low/capa*100,
                  level_high=.data$level_high/capa*100) %>%
    dplyr::mutate(mu=dplyr::case_when(.data$states_round_percent>.data$level_high ~ .data$mu - .data$penalty_high,
                                      .data$states_round_percent<=.data$level_low ~ .data$mu + .data$penalty_low,
                                      TRUE ~ .data$mu)) %>%
    dplyr::select(c("weeks","state_ref","mu","vu","states_round_percent"))

  return(vb)
}


#' Add fictive production and fictive load to avoid infeasabilities with binding constraints
#'
#' @param area A valid Antares area.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @keywords internal
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
#' @keywords internal
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

#' Restore initial scenario builder
#'
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param fictive_area Area for which to delete all scenario builder
#' @keywords internal
restoreScenarioBuilder <- function(opts, fictive_area){

  sb_file <- antaresRead::readIniFile(file.path(opts$studyPath, "settings", "scenariobuilder.dat"))

  if (length(names(sb_file))>0){
    assertthat::assert_that(length(names(sb_file))==1,
                            msg="There should be only one ruleset in scenario builder.")

    name_ruleset <- names(sb_file)[[1]]
    sb_file[[name_ruleset]] <- sb_file[[name_ruleset]][!grepl(fictive_area,names(sb_file$`Default Ruleset`))]

    antaresEditObject::writeIni(listData = sb_file,
                                pathIni = file.path(opts$studyPath, "settings", "scenariobuilder.dat"),
                                overwrite = TRUE, default_ext = ".dat")

    Sys.sleep(1)
  }

}
