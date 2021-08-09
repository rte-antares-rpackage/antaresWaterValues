#' Get reservoir capacity for concerned area
#' @param area The area concerned by the simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
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
#' @export

get_max_hydro <- function(area, opts=antaresRead::simOptions())
{
#import the table "standard credits" from "Local Data/ Daily Power and energy Credits"
max_hydro <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
if (hasName(max_hydro, "hstorPMaxHigh")) {
  max_hydro <- max_hydro[, max(hstorPMaxHigh)] * 168
} else {
  max_hydro <- max(max_hydro$generatingMaxPower) * 168    }
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
  tab <- apply(v,2,na.spline)
  tab <- tab[2:8,]
  reshaped_matrix <-rbind(reshaped_matrix,tab)
  last <-unlist(value_nodes_matrix[i,])
  }
  reshaped_matrix <- rbind(reshaped_matrix,value_nodes_matrix[1,])

return(reshaped_matrix)
}
