#' Read reservoir rule curves
#'
#' Read reservoir low and high levels of hydro object of the specified area.
#'
#' @inheritParams runWaterValuesSimulation
#' @returns A \code{dplyr::tibble()} with 4 columns : \code{"timeId"} (index of the week), \code{"level_low"}, \code{"level_avg"} and \code{"level_high"}. Values are given for the end of the last hour of the end of the week.
#'
#' @export
#'
readReservoirLevels <- function(area,
                                opts = antaresRead::simOptions()) {
  area = tolower(area)
  vars <- c("level_low", "level_avg", "level_high")
  reservoir = fread_antares(opts=opts,
                            file=file.path(opts$inputPath,
                                          "hydro","common",
                                          "capacity",
                                          paste0("reservoir_",area,".txt")))
  names(reservoir) = vars
  reservoir[, "timeId" := c(0,rep(seq_len(52), each = 7))]
  reservoir <- reservoir[, lapply(.SD,utils::tail,n=1), by = c("timeId"), .SDcols = vars]
  return(reservoir)
}

#' Get initial level
#'
#' Get initial level of hydro storage for the given area. Initial level is defined trough low and high reservoir levels for the first day.
#'
#' @inheritParams runWaterValuesSimulation
#'
#' @returns Double. Initial level in percentage, value between 0 and 100\%.
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
