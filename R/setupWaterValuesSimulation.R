#' Setup a simulation before running it for calculating Water Values,
#' used in \code{runWaterValuesSimulation}
#'
#' @param area The area concerned by the simulation.
#' @param fictive_area_name Name of the fictive area to create.
#' @param thermal_cluster Name of the thermal cluster to create.
#' @param overwrite If area or cluster already exists, overwrite them ?
#' @param remove_areas 	Character vector of area(s) to remove from the created district.
#' @param reset_hydro Boolean. True to reset hydro inflow to 0 before the simulation.
#' @param link_from area that will be linked to the created fictive area. If it's
#' \code{NULL} it will takes the area concerned by the simulation.
#' @param pumping Boolean. True to take into account the pumping.
#' @param max_load the maximum load to put it in the fictive areas.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param ... further arguments passed to or from other methods.
#' @return The result of antaresRead::simOptions
#'
setupWaterValuesSimulation <- function(area,
                                       fictive_area_name = paste0("watervalue_", area),
                                       thermal_cluster = "WaterValueCluster",
                                       overwrite = FALSE,
                                       remove_areas = NULL,
                                       opts = antaresRead::simOptions(),
                                       reset_hydro=T,
                                       link_from=NULL,pumping=F,max_load=100000000,...) {

  assertthat::assert_that(class(opts) == "simOptions")

  # Reset hydro storage
  if(reset_hydro){
    suppressWarnings(resetHydroStorage(area = area, opts = opts))
  }

  # Get hydro max power
  hydro_storage_max <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  hydro_storage_max <- rbind(
    hydro_storage_max, utils::tail(hydro_storage_max, 24)
  )

  # Reset Pumping power
  suppressWarnings(resetPumpPower(area = area, opts = opts))
  # Prepare thermal Cluster parameters
  if (utils::hasName(hydro_storage_max, "hstorPMaxHigh")) {
    prepro_modulation <- matrix(
      data = c(rep(1, times = 365 * 24 * 2),
               hydro_storage_max$hstorPMaxHigh/hydro_storage_max[, max(hydro_storage_max$hstorPMaxHigh)],
               rep(0, times = 365 * 24 * 1)),
      ncol = 4
    )
    time_series <- hydro_storage_max$hstorPMaxHigh
    nominalcapacity <- hydro_storage_max[, max(hydro_storage_max$hstorPMaxHigh)]
  } else {
    prepro_modulation <- matrix(
      data = c(rep(1, times = 365 * 24 * 2),
               hydro_storage_max$generatingMaxPower/hydro_storage_max[, max(hydro_storage_max$generatingMaxPower)],
               rep(0, times = 365 * 24 * 1)),
      ncol = 4
    )
    time_series <- hydro_storage_max$generatingMaxPower
    nominalcapacity <- hydro_storage_max[, max(hydro_storage_max$generatingMaxPower)]
  }

  # Chose the area to link
  from_area <- link_from
  if(is.null(link_from)){
    from_area <- area
  }


  fictive_areas <- fictive_area_name
  if(pumping){
    fictive_areas <- c(paste0(fictive_area_name,"_turb"),paste0(fictive_area_name,"_pump"))
  }


  for(fictive_area in fictive_areas){

    # Create fictive areas
    suppressWarnings({
      opts <- antaresEditObject::createArea(name = fictive_area, overwrite = overwrite, opts = opts)
    })

    # Create thermal cluster
    if(!grepl("_pump$", fictive_area)){
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
    }

    if(grepl("_pump$", fictive_area)){
      #add load
      max_pump <- hydro_storage_max$pumpingMaxPower
      antaresEditObject::writeInputTS(fictive_area, type = "load", data = max_pump)
    }


    # Create link
    suppressWarnings({
      opts <- antaresEditObject::createLink(
        from = from_area,
        to = fictive_area,
        propertiesLink = antaresEditObject::propertiesLinkOptions(transmission_capacities = "infinite"), #
        dataLink = NULL,
        overwrite = overwrite,
        opts = opts
      )
    })

  }#end fictive areas loop


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
      remove_area = fictive_areas,
      output = TRUE,
      overwrite = TRUE,
      opts = opts
    )
  })

  # Adjust thematic trimming
  settings_ini <- antaresEditObject::readIniFile(file.path(opts$studyPath, "settings", "generaldata.ini"))
  if (settings_ini$general$`thematic-trimming`){
    for (p in list("OV. COST","MRG. PRICE","BALANCE")){
      if (p %in% settings_ini$`variables selection`){
        idx <- which(settings_ini$`variables selection`== p)
        settings_ini$`variables selection`[[idx]] <- NULL
      }
      settings_ini$`variables selection` <- append(settings_ini$`variables selection`,
                                                       list(`select_var +`=p))
    }
    antaresEditObject::writeIni(settings_ini, file.path(opts$studyPath, "settings", "generaldata.ini"),overwrite=T)
  }

  return(opts)
}
