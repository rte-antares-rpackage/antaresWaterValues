#' Read Reservoir Levels
#'
#' @param area An 'antares' area.
#' @param timeStep Resolution of the data to import: weekly (default, a linear interpolation is done on the data), monthly (original data).
#' @param byReservoirCapacity Multiply the result by the reservoir capacity (if available).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a data.table
#' @export
#'
readReservoirLevels <- function(area,
                                timeStep = "weekly",
                                byReservoirCapacity = TRUE,
                                opts = antaresRead::simOptions(),
                                force_final_level = F,
                                final_level_egal_initial = F,
                                final_level = NULL) {
  assertthat::assert_that(class(opts) == "simOptions")
  if (antaresEditObject::is_antares_v7(opts = opts)) {
    res <- readReservoirLevelsV7(
      area = area,
      timeStep = timeStep,
      byReservoirCapacity = byReservoirCapacity,
      opts = opts
    )
  } else {
    res <- readReservoirLevelsV6(
      area = area,
      timeStep = timeStep,
      byReservoirCapacity = byReservoirCapacity,
      opts = opts
    )
  }
  if (force_final_level){
    if (final_level_egal_initial|is.null(final_level)){
      final_level <- readReservoirLevels(area, timeStep = "daily",
                                        byReservoirCapacity = FALSE,
                                        opts = opts)[[1,1]]*100
    }

    last_timeId <- res[nrow(res)]$timeId
    res <- res %>%
      dplyr::mutate(level_low = dplyr::if_else(.data$timeId==last_timeId,final_level/100,.data$level_low),
                    level_avg = dplyr::if_else(.data$timeId==last_timeId,final_level/100,.data$level_avg),
                    level_high = dplyr::if_else(.data$timeId==last_timeId,final_level/100,.data$level_high))
  }
  return(res)
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
#' @return a data.table

readReservoirLevelsV6 <- function(area, timeStep = "weekly", byReservoirCapacity = TRUE, opts = antaresRead::simOptions()) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop(
      "Package \"zoo\" must be installed to use this function.",
      call. = FALSE
    )
  }
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
  reservoir <- reservoir[, "timeId" := c(rep(seq_len(52), each = 7), 52)]
  reservoir <- reservoir[, lapply(.SD, mean), by = c("timeId"), .SDcols = vars]
  if (byReservoirCapacity) {
    reservoirCapacity <- get_reservoir_capacity(area = area, opts = opts)
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
#' @return a data.table
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
    reservoir[, "timeId" := seq_len(nrow(reservoir))]
    reservoir[, date := NULL]
  }
  if (timeStep == "monthly") {
    data.table::setcolorder(x = reservoir, neworder = c("date", vars))
    reservoir <- reservoir[order(date)]
    reservoir[, "timeId" := format(date, format = "%Y-%m-01")]
    reservoir <- reservoir[, lapply(.SD,utils::tail,n=1), by = c("timeId"), .SDcols = vars]
    reservoir[, "timeId" := seq_len(nrow(reservoir))]
  }
  if (timeStep == "weekly") {
    reservoir[, "timeId" := c(rep(seq_len(52), each = 7), 52)]
    reservoir <- reservoir[, lapply(.SD,utils::tail,n=1), by = c("timeId"), .SDcols = vars]
  }
  if (byReservoirCapacity) {
    reservoirCapacity <- get_reservoir_capacity(area = area, opts = opts)
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
