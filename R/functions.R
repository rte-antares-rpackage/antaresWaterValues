#' Generate the list of constraint values of the link between the fictive area and the real one
#' @param area The area concerned by the simulation.
#' @param nb_disc_stock Number of simulation to launch, a vector of energy constraint.
#' @param pumping Boolean. True to take into account the pumping.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
constraint_generator <- function(area,nb_disc_stock,pumping=F,opts)
  {
  max_hydro <- get_max_hydro(area,opts)
  res_cap <- get_reservoir_capacity(area,opts)
  max_app <- max( antaresRead::readInputTS(hydroStorage = area , timeStep="weekly")$hydroStorage)
  maxi <- min(max_hydro$turb+max_app,res_cap)
  mini <- -max_hydro$pump
  if(pumping){
  constraint_values <- seq(from = mini, to = maxi, length.out = nb_disc_stock)
  constraint_values <- round(constraint_values, 3)
  constraint_values[which(abs(constraint_values)==min(abs(constraint_values)))] <- 0
  }else{
    constraint_values <- seq(from = 0, to = maxi, length.out = nb_disc_stock)
    constraint_values <- round(constraint_values, 3)
  }

return(constraint_values)
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
#' @importFrom  ggplot2 ggplot geom_col scale_fill_viridis_d facet_grid scale_fill_brewer
#' @importFrom  antaresRead setSimulationPath readAntares
#' @importFrom dplyr select
#' @importFrom grDevices rgb
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



#' Get reservoir capacity for concerned area
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @importFrom antaresEditObject readIniFile
#' @export

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
#' @importFrom antaresRead readInputTS
#' @importFrom utils hasName
#' @export

get_max_hydro <- function(area, opts=antaresRead::simOptions())
{
#import the table "standard credits" from "Local Data/ Daily Power and energy Credits"
max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
if (utils::hasName(max_hydro, "hstorPMaxHigh")) {
  max_turb <- max_hydro[, max(hstorPMaxHigh)] * 168
} else {
  max_turb <- max(max_hydro$generatingMaxPower) * 168
  max_pump <- max(max_hydro$pumpingMaxPower) * 168
}
max_hydro <- list()
max_hydro$pump <- max_pump
max_hydro$turb <- max_turb
class(max_hydro) <- "max turbining and pumping weekly energy"
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


#------------- get turbaned capacity from reward table-----
#' Extract values of turbined energy from simulation names.
#' @param reward_dt reward data.table. Obtained using the function \code{get_Reward()}
#' @param sim_name_pattern the name of simulations used in \code{runWaterValuesSimulation()}
#' @export

names_reward <-function(reward_dt,sim_name_pattern="weekly_water_amount_"){
  j <- 3
  names <- names(reward_dt)
  if (names[1]=="weekly_water_amount_0")
    {j <- 1}
  values <- gsub(sim_name_pattern,"",names[3:length(names)])
  values <- as.numeric(sub(",", ".", values, fixed = TRUE))
  values <- as.integer(values)
  return(values)
}


#------------- to antares format -------
#' Convert water values to Antares format
#'
#' This function converts water values generated by \code{meanGridLayer} or
#' \code{waterValues} to the format expected by Antares: a 365*101 matrix, where
#' the rows are the 365 days of the year and the columns are round percentage values
#' ranging from 0 to 100 assessing the reservoir level.
#' Since \code{meanGridLayer} and \code{waterValues} output weekly values for an
#' arbitrary number of reservoir levels, interpolation is performed on both scales
#' in order to fit the desired format.
#'
#' @param data A 7-column data.table generated by \code{watervalues::Grid_Matrix()}
#'
#' @return A 365*101 numeric matrix
#' @importFrom data.table data.table CJ dcast
#' @importFrom zoo na.spline
#' @export

to_Antares_Format <- function(data){

  # rescale levels to round percentages ranging from 0 to 100
  states_ref <- data[, .SD[1], by = statesid, .SDcols = "states"]
  states_ref[, states_percent := 100*states/max(states)]

  nearest_states <- states_ref$statesid[sapply(0:100, function(x) which.min(abs(x - states_ref$states_percent)))]

  states_ref_0_100 <- data.table(
    states_round_percent = 0:100,
    statesid = nearest_states
  )

  res <- CJ(weeks = unique(data$weeks), states_round_percent = 0:100)

  res[states_ref_0_100, on = "states_round_percent", statesid := i.statesid]

  res[data, on = c("weeks", "statesid"), vu := i.vu]

  # reshape
  value_nodes_matrix <- dcast(
    data = res,
    formula = weeks ~ states_round_percent,
    value.var = "vu"
  )

  value_nodes_matrix$weeks <- NULL

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
  reshaped_matrix <- rbind(reshaped_matrix,value_nodes_matrix[1,])

return(reshaped_matrix)
}

#' Release parallel computing clusters
#'
#' @export
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
