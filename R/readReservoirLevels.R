#' Read Reservoir Levels
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: weekly (default, a linear interpolation is done on the data), monthly (original data).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'   
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead getAreas
#' @importFrom zoo na.approx
#' @importFrom utils head
#'
#' @return a data.table
#' @export
#'
# @examples
readReservoirLevels <- function(area, timeStep = "weekly", opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  timeStep <- match.arg(arg = timeStep, choices = c("weekly", "monthly"))
  if (!area %in% antaresRead::getAreas(opts = opts)) 
    stop("Not a valid area!")
  
  vars <- c("level_low", "level_avg", "level_high")
  reservoir <- data.table::fread(
    input = file.path(opts$inputPath , sprintf("hydro/common/capacity/reservoir_%s.txt", area)),
    col.names = vars
  )
  full_year <- data.table(
    date = seq.Date(from = as.Date(opts$start), by = "day", length.out = 364)
  )
  reservoir <- reservoir[, date := full_year[format(date, "%d") == "01"][order(format(date, "%m")), c(date)]]
  if (timeStep == "monthly") {
    data.table::setcolorder(x = reservoir, neworder = c("date", vars))
    reservoir <- reservoir[order(date)]
    return(reservoir)
  }
  reservoir <- merge(x = full_year, y = reservoir, by = "date", all.x = TRUE)
  interpolation <- function(x) {
    utils::head(zoo::na.approx(c(x, x[1]), na.rm = FALSE), -1)
  }
  reservoir <- reservoir[, (vars) := lapply(.SD, interpolation), .SDcols = vars]
  reservoir <- reservoir[, timeId := rep(seq_len(52), each = 7)]
  reservoir <- reservoir[, lapply(.SD, mean), by = timeId, .SDcols = vars]
  reservoir
}
