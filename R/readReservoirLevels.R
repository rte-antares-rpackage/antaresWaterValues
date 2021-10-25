#' Read Reservoir Levels
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: weekly (default, a linear interpolation is done on the data), monthly (original data).
#' @param byReservoirCapacity Multiply the result by the reservoir capacity (if available).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead getAreas
#' @importFrom antaresEditObject is_antares_v7
#' @importFrom zoo na.approx
#' @importFrom utils head
#'
#' @return a data.table
#' @export
#'
readReservoirLevels <- function(area,
                                timeStep = "weekly",
                                byReservoirCapacity = TRUE,
                                opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (is_antares_v7(opts = opts)) {
    readReservoirLevelsV7(
      area = area,
      timeStep = timeStep,
      byReservoirCapacity = byReservoirCapacity,
      opts = opts
    )
  } else {
    readReservoirLevelsV6(
      area = area,
      timeStep = timeStep,
      byReservoirCapacity = byReservoirCapacity,
      opts = opts
    )
  }
}


#' Read Reservoir Levels for Antares version 6.
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: weekly (default, a linear interpolation is done on the data), monthly (original data).
#' @param byReservoirCapacity Multiply the result by the reservoir capacity (if available).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead getAreas
#' @importFrom antaresEditObject is_antares_v7
#' @importFrom zoo na.approx
#' @importFrom utils head
#'
#' @return a data.table
#' @export

readReservoirLevelsV6 <- function(area, timeStep = "weekly", byReservoirCapacity = TRUE, opts = antaresRead::simOptions()) {
  timeStep <- match.arg(arg = timeStep, choices = c("weekly", "monthly"))
  if (!area %in% antaresRead::getAreas(opts = opts))
    stop("Not a valid area!")
  path <- file.path(opts$inputPath , sprintf("hydro/common/capacity/reservoir_%s.txt", area))
  vars <- c("level_low", "level_avg", "level_high")
  if (!file.exists(path) || file.size(path) == 0) {
    reservoir <- data.table::data.table(matrix(rep(0, 3*12), ncol = 3, dimnames = list(NULL, vars)))
  } else {
    reservoir <- data.table::fread(input = path, col.names = vars)
  }
  full_year <- data.table(
    date = seq.Date(from = as.Date(opts$start), by = "day", length.out = 365)
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
  reservoir <- reservoir[, timeId := c(rep(seq_len(52), each = 7), 52)]
  reservoir <- reservoir[, lapply(.SD, mean), by = timeId, .SDcols = vars]
  if (byReservoirCapacity) {
    reservoirCapacity <- getReservoirCapacity(area = area, opts = opts)
    if (!is.null(reservoirCapacity)) {
      vars <- c("level_low", "level_avg", "level_high")
      reservoir <- reservoir[,
                             (vars) := lapply(.SD, function(x) {x * reservoirCapacity}),
                             .SDcols = vars
      ]
    }
  }
  reservoir[]
}

#' Read Reservoir Levels for antares version 7.
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: weekly (default, a linear interpolation is done on the data), monthly (original data).
#' @param byReservoirCapacity Multiply the result by the reservoir capacity (if available).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom antaresRead getAreas
#' @importFrom antaresEditObject is_antares_v7
#' @importFrom zoo na.approx
#' @importFrom utils head tail
#'
#' @return a data.table
#' @export
readReservoirLevelsV7 <- function(area, timeStep = "weekly", byReservoirCapacity = TRUE, opts = antaresRead::simOptions()) {
  timeStep <- match.arg(arg = timeStep, choices = c("weekly", "monthly", "daily"))
  if (!area %in% antaresRead::getAreas(opts = opts))
    stop("Not a valid area!")
  path <- file.path(opts$inputPath , sprintf("hydro/common/capacity/reservoir_%s.txt", area))
  vars <- c("level_low", "level_avg", "level_high")
  if (!file.exists(path) || file.size(path) == 0) {
    reservoir <- data.table::data.table(matrix(rep(0, 3*365), ncol = 3, dimnames = list(NULL, vars)))
  } else {
    reservoir <- data.table::fread(input = path, col.names = vars)
  }
  full_year <- data.table(
    date = seq.Date(from = as.Date(opts$start), by = "day", length.out = nrow(reservoir))
  )
  reservoir <- reservoir[, date := full_year$date]
  if (timeStep == "daily") {
    setnames(x = reservoir, old = "date", new = "timeId")
  }
  if (timeStep == "monthly") {
    data.table::setcolorder(x = reservoir, neworder = c("date", vars))
    reservoir <- reservoir[order(date)]
    reservoir[, timeId := format(date, format = "%Y-%m-01")]
    reservoir <- reservoir[, lapply(.SD,tail,n=1), by = timeId, .SDcols = vars]
  }
  if (timeStep == "weekly") {
    reservoir[, timeId := c(rep(seq_len(52), each = 7), 52)]
    reservoir <- reservoir[, lapply(.SD,tail,n=1), by = timeId, .SDcols = vars]
  }
  if (byReservoirCapacity) {
    reservoirCapacity <- getReservoirCapacity(area = area, opts = opts)
    if (!is.null(reservoirCapacity)) {
      vars <- c("level_low", "level_avg", "level_high")
      reservoir <- reservoir[
        , (vars) := lapply(.SD, function(x) {x * reservoirCapacity}),
        .SDcols = vars

      ]
    }
  }
  reservoir[]
}
