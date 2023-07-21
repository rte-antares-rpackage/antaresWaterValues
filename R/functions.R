#' Restore the Pumping power series, used in \code{runWaterValuesSimulation}
#'
#' @param area A valid Antares area.
#' @param path Path to a manual backup.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param silent Boolean. True to run without messages.
#' @return An updated list containing various information about the simulation
restorePumpPower <- function(area, path = NULL, opts = antaresRead::simOptions(),silent=F) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath

  if (is.null(path)) {
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
      from = path,
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
#' @return An updated list containing various information about the simulation.
resetPumpPower <- function(area, path = NULL, opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOptions")
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))

  # Input path
  inputPath <- opts$inputPath






  # Pump power ----
  if (is.null(path)) {
    path_test <-  file.path(inputPath, "hydro", "common","capacity", paste0("backup_maxpower_",area,".txt"))

    #In case there is mod_backup from an interrupted simulation
    if (file.exists(path_test)) {
      file.copy(
        from = path_test,
        to = file.path(inputPath, "hydro", "common","capacity", paste0("maxpower_",area,".txt")),
        overwrite = TRUE
      )
      unlink(x=path_test)
    }


    path_pump_power <- file.path(inputPath, "hydro", "common","capacity",  paste0("maxpower_",area,".txt"))
  } else {
    path_pump_power <- path
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






#--------- Reporting data---------------
#' Plot simulation variables comparison and real Ov. cost (for watervalues)
#'
#' @param simulations list of simulation names.
#' @param timeStep Resolution of the data to import.
#' @param type "area" to import areas and "district" to import districts.
#' @param district_list list of district to plot. assign "all" to import all districts.
#' @param area_list list of area to plot. assign "all" to import all areas.
#'  that contains the all domain to study.
#' @param mcyears precise the MC year to plot.
#' #' Null plot the synthesis. Default NULL
#' @param plot_var list of variables to plot.
#' @param watervalues_areas list of areas name that used water values.

#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param ... further arguments passed to or from other methods.
#' @import data.table
#' @return a \code{ggplot} or \code{data.table} object
#' @export



report_data <- function(simulations,type="area",district_list="all",area_list="all",timeStep="annual",
                        mcyears,opts,plot_var,watervalues_areas,...) {

  {column_names <- c("sim_name","area", "timeId", "time","OV. COST", "OP. COST","MRG. PRICE", "CO2 EMIS.", "BALANCE",
                     "ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR","WIND", "SOLAR", "NUCLEAR",
                     "LIGNITE","COAL",  "GAS", "OIL","MIX. FUEL","MISC. DTG","H. STOR",
                     "H. PUMP","H. LEV", "H. INFL", "H. OVFL","H. VAL", "H. COST","UNSP. ENRG",
                     "SPIL. ENRG", "LOLD","LOLP", "AVL DTG", "DTG MRG","MAX MRG", "NP COST","NODU",
                     "sim_name","total_hydro_cost","Real OV. COST")}
  if (is.null(simulations)) return(NULL)
  data <- data.table(matrix(nrow = 0, ncol = length(column_names)))
  setnames(data,column_names)


  for(simulation_name in simulations){
    tmp_opt <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = simulation_name)

    if(length(watervalues_areas)>0)
    {
      row_h <- antaresRead::readAntares(areas =watervalues_areas , timeStep = timeStep ,
                                        mcYears = mcyears, opts = tmp_opt,showProgress = F)

      row_h$stockDiff <- 0
      row_h$hydro_price <- 0
      row_h$hydro_stockDiff_cost <- 0
      row_h$hydro_cost <- 0
      row_h$total_hydro_cost <- 0

      for (area_name in watervalues_areas){

        hydro_list <- hydro_cost(area=area_name,mcyears=mcyears,simulation_name,opts)

        row_h[area==area_name,stockDiff:=hydro_list$stockDiff]
        row_h[area==area_name,hydro_price:=hydro_list$hydro_price]
        row_h[area==area_name,hydro_stockDiff_cost:=hydro_list$hydro_stockDiff_cost]
        row_h[area==area_name,hydro_cost:=hydro_list$hydro_cost]
        row_h[area==area_name,total_hydro_cost:= hydro_list$total_hydro_cost]

        if(area_name==watervalues_areas[length(watervalues_areas)])
        {
          stockDiff <- sum(row_h$stockDiff)
          hydro_price <- mean(row_h$hydro_price)
          hydro_stockDiff_cost <- sum(row_h$hydro_stockDiff_cost)
          hydro_cost <- sum(row_h$hydro_cost)
          total_hydro_cost <- sum(row_h$total_hydro_cost)


        }}
    }else{
      stockDiff <- 0
      hydro_price <- 0
      hydro_stockDiff_cost <- 0
      hydro_cost <- 0
      total_hydro_cost <- 0
    }


    if(type=="district") {
      row <- antaresRead::readAntares(districts = district_list, timeStep = timeStep ,
                                      mcYears = mcyears, opts = tmp_opt,showProgress = F)

      row$stockDiff <- stockDiff
      row$hydro_price <- hydro_price
      row$hydro_stockDiff_cost <- hydro_stockDiff_cost
      row$hydro_cost <- hydro_cost
      row$total_hydro_cost <- total_hydro_cost

    }else{
      row <- antaresRead::readAntares(areas = area_list, timeStep = timeStep ,
                                      mcYears = mcyears, opts = tmp_opt,showProgress = F)
      row <- dplyr::left_join(row,row_h)

    }


    row$sim_name <- stringr::str_trunc(simulation_name, 20, "left")

    row$`Real OV. COST` <- row$`OV. COST`-row$total_hydro_cost



    data <- base::rbind(data,row,fill=T)
  }

  return(data)



}



#' Get reservoir capacity for concerned area, used in different functions
#'
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}

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


#' Get max hydro power that can be generated in a week, used in different functions
#'
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param timeStep among "hourly", "daily" and "weekly"

get_max_hydro <- function(area, opts=antaresRead::simOptions(),timeStep="hourly")
{
#import the table "standard credits" from "Local Data/ Daily Power and energy Credits"
max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
if (utils::hasName(max_hydro, "hstorPMaxHigh")) {
  max_turb <- max_hydro[, max(hstorPMaxHigh)] * 168
} else {
  if (timeStep=="hourly"){
    max_turb <- max_hydro$generatingMaxPower
    max_pump <- max_hydro$pumpingMaxPower
  } else if (timeStep=="daily"){
    max_hydro$day <- (max_hydro$timeId-1)%/%24+1
    max_hydro <- dplyr::summarise(dplyr::group_by(max_hydro,day),turb=sum(generatingMaxPower),
                           pump=sum(pumpingMaxPower))
    max_turb <- max_hydro$turb
    max_pump <- max_hydro$pump
  } else if (timeStep=="weekly"){
    max_hydro$week <- (max_hydro$timeId-1)%/%168+1
    max_hydro <- dplyr::summarise(dplyr::group_by(max_hydro,week),turb=sum(generatingMaxPower),
                           pump=sum(pumpingMaxPower))
    max_turb <- max_hydro$turb
    max_pump <- max_hydro$pump
  } else {message("timeStep not supported, change to hourly, weekly or daily")}

}
max_hydro <- data.frame(timeId=seq(nrow(max_hydro)))
max_hydro$pump <- max_pump
max_hydro$turb <- max_turb
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
#'
#' @return A 365*101 numeric matrix
#' @export

to_Antares_Format <- function(data,penalty_level_low,penalty_level_high,constant=T){

  data <- dplyr::group_by(data, weeks) %>%
    dplyr::arrange(states) %>%
    dplyr::mutate(vu_pen=dplyr::if_else(is.na(.data$vu_pen),dplyr::lead(.data$vu_pen),.data$vu_pen),
           vu=dplyr::if_else(is.na(vu),dplyr::lead(vu),vu))
  data <- as.data.table(data)

  # rescale levels to round percentages ranging from 0 to 100
  states_ref <- data[, .SD[1], by = statesid, .SDcols = "states"]
  states_ref <- dplyr::mutate(states_ref, states_percent = 100*states/max(states))

  nearest_states <- states_ref$statesid[sapply(0:100, function(x) min(which(x-states_ref$states_percent<=0)))]

  states_ref_0_100 <- data.table(
    states_round_percent = 0:100,
    statesid = nearest_states
  )

  res <- CJ(weeks = unique(data$weeks), states_round_percent = 0:100)

  res[states_ref_0_100, on = "states_round_percent", statesid := i.statesid]

  max_state <- max(states_ref$states)

  res <- res %>% dplyr::left_join(dplyr::select(data,"weeks","statesid","vu_pen","level_low","level_high"),
                           by = c("weeks", "statesid")) %>%
    dplyr::mutate(level_low=level_low/max_state*100,
           level_high=level_high/max_state*100,
           vu=dplyr::case_when(states_round_percent>level_high ~ .data$vu_pen - penalty_level_high,
                        states_round_percent<level_low ~ .data$vu_pen + penalty_level_low,
                            TRUE ~ .data$vu_pen)) %>%
    dplyr::mutate(weeks=dplyr::if_else(weeks>=2,weeks-1,52))


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
