#' Read reservoir rule curves
#'
#' Read reservoir low and high levels of hydro object of the specified area.
#'
#' @inheritParams runWaterValuesSimulation
#' @returns A \code{dplyr::tibble()} with 4 columns : \code{"timeId"} (index of the week), \code{"level_low"}, \code{"level_avg"} and \code{"level_high"}. Values are given for the end of the last hour of the end of the week.
#'
#' @export
#'
readReservoirLevels <- function(area,opts) {
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
  initial_level <- readReservoirLevels(area,
                                     opts = opts)[1,]
  assertthat::assert_that(initial_level$level_low==initial_level$level_high,
                          msg = "Initial level is not defined properly in the Antares study. Please correct it by setting level_low and level_high equals for the first day of the year.")
  return(initial_level$level_low*100)
}

#' Get initial level year per year
#'
#' Get initial level of hydro storage for the given area for each MC year based on the scenario builder. If random in the scenario builder, the initial level is defined trough low and high reservoir levels for the first day.
#'
#' @inheritParams runWaterValuesSimulation
#'
#' @returns Vector. Initial level for each MC year in percentage, value between 0 and 100\%.
#'
#' @export
get_initial_level_year_per_year <- function(area,opts){
  initial_level = get_initial_level(area,opts)

  sb_hl = antaresEditObject::readScenarioBuilder(
    as_matrix = T,
    opts = opts
  )$hl[area, ]*100

  sb_hl[is.na(sb_hl)] = initial_level
  return(sb_hl)
}
