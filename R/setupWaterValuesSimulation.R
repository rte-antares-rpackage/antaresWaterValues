#' Setup a simulation before running it for calculating Water Values
#'
#' @param area The area concerned by the simulation.
#' @param fictive_area Name of the fictive area to create.
#' @param thermal_cluster Name of the thermal cluster to create.
#' @param overwrite If area or cluster already exists, overwrite them ?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return The result of antaresRead::simOptions
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions readInputTS
#' @importFrom antaresEditObject createArea createCluster createDistrict createLink propertiesLinkOptions updateGeneralSettings
#' @import data.table
#' @importFrom utils tail hasName
#'
#'
# @examples
setupWaterValuesSimulation <- function(area,
                                       fictive_area = paste0("watervalue_", area),
                                       thermal_cluster = "WaterValueCluster",
                                       overwrite = FALSE,
                                       opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")

  # Create fictive area
  suppressWarnings({
    opts <- antaresEditObject::createArea(name = fictive_area, overwrite = overwrite, opts = opts)
  })

  # Reset hydro storage
  suppressWarnings(resetHydroStorage(area = area, opts = opts))

  # Get hydro max power
  hydro_storage_max <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  hydro_storage_max <- rbind(
    hydro_storage_max, tail(hydro_storage_max, 24)
  )

  # Create thermal cluster
  if (utils::hasName(hydro_storage_max, "hstorPMaxHigh")) {
    prepro_modulation <- matrix(
      data = c(rep(1, times = 365 * 24 * 2),
               hydro_storage_max[, c(hstorPMaxHigh)]/hydro_storage_max[, max(hstorPMaxHigh)],
               rep(0, times = 365 * 24 * 1)),
      ncol = 4
    )
    time_series <- hydro_storage_max[, list(hstorPMaxHigh)]
    nominalcapacity <- hydro_storage_max[, max(hstorPMaxHigh)]
  } else {
    prepro_modulation <- matrix(
      data = c(rep(1, times = 365 * 24 * 2),
               hydro_storage_max[, c(generatingMaxPower)]/hydro_storage_max[, max(generatingMaxPower)],
               rep(0, times = 365 * 24 * 1)),
      ncol = 4
    )
    time_series <- hydro_storage_max[, list(generatingMaxPower)]
    nominalcapacity <- hydro_storage_max[, max(generatingMaxPower)]
  }

  suppressWarnings({
    opts <- antaresEditObject::createCluster(
      area = fictive_area,
      cluster_name = thermal_cluster,
      group = "other", unitcount = "1",
      time_series = time_series,
      nominalcapacity = nominalcapacity,
      prepro_modulation = prepro_modulation,
      `min-down-time` = "1",
      `marginal-cost` = 0.01,
      `market-bid-cost` = 0.01,
      overwrite = overwrite,
      opts = opts
    )
  })

  # Create link
  suppressWarnings({
    opts <- antaresEditObject::createLink(
      from = area,
      to = fictive_area,
      propertiesLink = antaresEditObject::propertiesLinkOptions(transmission_capacities = "infinite"), #
      dataLink = NULL,
      overwrite = overwrite,
      opts = opts
    )
  })


  # Activate output year by year
  suppressWarnings({
    antaresEditObject::updateGeneralSettings(year.by.year = TRUE, opts = opts)
  })


  # Create a water values district
  suppressWarnings({
    antaresEditObject::createDistrict(
      name = "water values district",
      caption = "water values district",
      comments = "Used for calculate water values",
      apply_filter = "add-all",
      output = TRUE,
      overwrite = TRUE,
      opts = opts
    )
  })


  opts
}
