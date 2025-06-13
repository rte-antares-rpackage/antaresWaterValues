#' Read Reservoir Levels
#'
#' @param area An 'antares' area.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a data.table
#' @export
#'
readReservoirLevels <- function(area,
                                opts = antaresRead::simOptions()) {
  area = tolower(area)
  vars <- c("level_low", "level_avg", "level_high")
  reservoir = antaresRead:::fread_antares(opts=opts,
                                          file=file.path(opts$inputPath,
                                                        "hydro","common",
                                                        "capacity",
                                                        paste0("reservoir_",area,".txt")))
  names(reservoir) = vars
  reservoir[, "timeId" := c(0,rep(seq_len(52), each = 7))]
  reservoir <- reservoir[, lapply(.SD,utils::tail,n=1), by = c("timeId"), .SDcols = vars]
  return(reservoir)
}

#' Get initial level of an area
#'
#' @param area An 'antares' area.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export

get_initial_level <- function(area,opts){
  area = tolower(area)
  final_level <- readReservoirLevels(area,
                                     opts = opts)[1,]
  assertthat::assert_that(final_level$level_low==final_level$level_high,
                          msg = "Initial level is not defined properly in the Antares study. Please correct it by setting level_low and level_high equals for the first day of the year.")
  final_level <- final_level$level_low*100
}
