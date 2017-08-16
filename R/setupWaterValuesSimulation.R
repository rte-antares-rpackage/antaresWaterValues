#' Setup a simulation befor running it for calculating Water Values
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
#' @import antaresEditObject
#'
#'
# @examples
setupWaterValuesSimulation <- function(area, 
                                       fictive_area = paste0("WaterValue_", area), 
                                       thermal_cluster = "WaterValueCluster",
                                       overwrite = FALSE, 
                                       opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  
  # Create fictive area
  antaresEditObject::createArea(name = fictive_area, overwrite = overwrite, opts = opts)
  
  # Reset hydro storage
  resetHydroStorage(area = area, opts = opts)
  
  # Get hydro max power
  hydro_storage_max <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly")
  
  # Create thermal cluster
  antaresEditObject::createCluster(
    area = area, cluster_name = thermal_cluster,
    group = "other", unitcount = "1",
    time_series = hydro_storage_max[, list(hstorPMaxHigh)],
    nominalcapacity = hydro_storage_max[, max(hstorPMaxHigh)],
    `min-down-time` = "1", `marginal-cost` = 0.01,
    `market-bid-cost` = 0.01, overwrite = overwrite
  )
  
  # Create link
  dataLink <- matrix(
    data = c(rep(0, 8760), rep(hydro_storage_max[, max(hstorPMaxHigh)], 8760), rep(0, 8760*3)),
    ncol = 5
  )
  antaresEditObject::createLink(
    from = area, to = fictive_area, 
    propertiesLink = propertiesLinkOptions(transmission_capacities = "infinite"), 
    dataLink = dataLink, overwrite = overwrite
  )
  
  
  # Activate output year by year
  updateGeneralSettings(year.by.year = TRUE)
  
  

  
  antaresRead::simOptions()
}