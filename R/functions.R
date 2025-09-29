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
  hydro_ini <- antaresRead::readIni(file.path("input", "hydro", "hydro.ini"),opts=opts)
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
  area = tolower(area)
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
  max_hydro <- data.frame(timeId=seq_len(nrow(max_hydro)))
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
  area = tolower(area)
  suppressWarnings(inflow <- antaresRead::readInputTS(hydroStorage = area , timeStep="hourly", opts=opts))
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

#' Get objective values of the optimization problem of each week and each scenario for a given simulation \code{simu}, mainly used in \code{get_Reward()} to build reward functions.
#'
#' @inheritParams get_local_reward
#' @inheritParams runWaterValuesSimulation
#'
#' @return A \code{dplyr::tibble()} with 3 columns : \code{"timeId"}, \code{"mcYear"} and \code{"ov_cost"}.
#'
#' @export
get_weekly_cost <- function(simu,mcyears,expansion=FALSE){
  if (!is_api_study(simu)){
    path <- paste0(simu$simPath)
    all_files <- list.files(path)
    assertthat::assert_that(sum(stringr::str_detect(all_files,"criterion")) >= 52*length(mcyears))
  }
  if (!expansion){
    cost <- antaresRead::readAntares(districts = "water values district", mcYears = mcyears,
                                     timeStep = "hourly", opts = simu, select=c("OV. COST"))
    cost$week <- (cost$timeId-1)%/%168+1
    cost <- dplyr::summarise(dplyr::group_by(cost,.data$week,.data$mcYear),
                             ov_cost=sum(.data$`OV. COST`)) %>%
      dplyr::rename("timeId"="week")
  } else {
    cost <- data.frame(tidyr::expand_grid(mcYear=mcyears,timeId=1:52))%>%
        dplyr::mutate(ov_cost=0,cost_xpansion=0)
    for (week in 1:52){
      for (scenario in mcyears){
        if (is_api_study(simu)){
          file = antaresRead::api_get(opts=simu,endpoint=paste0(simu$study_id,
                    "/raw?path=output%2F",simu$simOutputName,"%2Fcriterion-",scenario,"-",week,"--optim-nb-1"),
                                                  parse_result = "text",encoding = "UTF-8")
        } else{
          path <- all_files[stringr::str_detect(all_files,paste0("criterion-",
                                                                 scenario,"-",
                                                                 week,"-"))][[1]]
          file = utils::read.delim(paste0(simu$simPath,"/",path),
                                   header = FALSE)
        }
        cost_xpansion <- as.numeric(strsplit(file[[1]],":")[[1]][[2]])
        cost[(cost$timeId==week)&(cost$mcYear==scenario),4] <- cost_xpansion
      }
    }
    cost <- cost %>%
      dplyr::mutate(ov_cost =.data$cost_xpansion-.data$ov_cost) %>%
      dplyr::select(-c("cost_xpansion"))
  }

  return(data.table(cost))
}

clear_scenario_builder <- function(opts){
  if (!is_api_study(opts)){
    sb_file <- antaresRead::readIni(file.path("settings", "scenariobuilder.dat"),opts=opts,default_ext = ".dat")
    if (length(names(sb_file))>0){
      assertthat::assert_that(length(names(sb_file))==1,
                              msg="There should be only one ruleset in scenario builder.")
      name_ruleset <- names(sb_file)[[1]]
      sb_file[[name_ruleset]] <- sb_file[[name_ruleset]][!stringr::str_detect(names(sb_file[[name_ruleset]]),"bc,watervalues")]
      antaresEditObject::writeIni(listData = sb_file,
                                  opts=opts,
                                  pathIni = file.path("settings", "scenariobuilder.dat"),
                                  overwrite = TRUE, default_ext = ".dat")
      Sys.sleep(1)
    }
  }
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
to_Antares_Format <- function(data,constant=TRUE){

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

  for (i in seq_len(nrow(vb))){
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
    dplyr::mutate(states=round(.data$states,6)) %>%
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
#' @param load Matrix with 8760 rows that contains backup load for the area
#' @param misc_gen Matrix with 8760 rows that contains backup misc generation for the area
#'
#' @return An updated list containing various information about the simulation.
#' @keywords internal
add_fictive_fatal_prod_demand <- function(area, opts, load, misc_gen){
  max_hydro <- get_max_hydro(area=area,opts=opts,timeStep="hourly") %>%
    dplyr::select(-c("timeId")) %>% max()

  antaresEditObject::writeInputTS(data = load + max_hydro, type="load", area=area, opts=opts)

  misc_gen[, 6] = misc_gen[, 6] + max_hydro
  antaresEditObject::writeMiscGen(data = misc_gen, area = area, opts=opts)

}

#' Restore load and misc gen time series
#'
#' @param area A valid Antares area.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param load Matrix with 8760 rows that contains backup load for the area
#' @param misc_gen Matrix with 8760 rows that contains backup misc generation for the area
#' @return An updated list containing various information about the simulation.
#' @keywords internal
restore_fictive_fatal_prod_demand <- function(area, opts,
                                              load, misc_gen) {
  antaresEditObject::writeInputTS(data = load, type="load", area=area, opts=opts)

  antaresEditObject::writeMiscGen(data = misc_gen, area = area, opts=opts)
}

is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

check_area_name <- function(area, opts) {
  areaList <- antaresRead::getAreas(opts = opts)
  if (!tolower(area) %in% areaList)
    stop("'", area, "' is not a valid area name, possible names are: ", paste(areaList, collapse = ", "), call. = FALSE)
}

#' @importFrom utils URLencode
#' @importFrom shiny isRunning
fread_antares <- function(opts, file, ...) {
  if (identical(opts$typeLoad, "api")) {
    file <- gsub("\\.txt$", "", file)
    response <- antaresRead::api_get(
      opts = opts,
      endpoint = I(file),
      query = list(formatted = FALSE)
    )
    suppressWarnings(
      tryCatch(fread(response, ...), error = function(e){
        if(isRunning())
          e <- as.character(e)
        message(file)
        message(e)
      }))
  } else {
    suppressWarnings(
      fread(file, ...))
  }
}
