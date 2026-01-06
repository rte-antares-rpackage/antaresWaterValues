#' Compute Bellman values for several storage with plaia
#'
#' For each storage, reward functions are evaluated and Bellman values are computed with plaia.
#' Results are written directly to the output folder.
#'
#' @inheritParams runWaterValuesSimulationMultiStock
#' @inheritParams Grid_Matrix
#' @param penalty_final_level Double. Penalties for both rule curves to constrain final level.
#' @param name_sim Character. Name of the simulation to launch
#' @param n_controls Integer. Number of controls to evaluate reward functions.
#' @return List of watervalues matrix
#' @export
#'
getBellmanValuesWithPlaia <- function(opts,
                                      mcyears,
                                      list_areas,
                                      force_final_level,
                                      penalty_final_level,
                                      penalty_low,
                                      penalty_high,
                                      name_sim,
                                      n_controls) {

  list_areas = tolower(list_areas)

  list_backup = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]
    check_area_name(area = area, opts = opts)

    list_backup[[j]] = getBackupData(area,mcyears,opts)
  }

  list_watervalues = list()

  year_by_year = opts$parameters$general$`year-by-year`
  synthesis = opts$parameters$output$synthesis
  storenewset = opts$parameters$output$storenewset

  assertthat::assert_that(is_api_study(opts),msg="Study must be in API mode to launch simulations.")

  try(antaresRead::api_post(opts=opts,
                            endpoint = paste0(opts$study_id,"/extensions/xpansion")),
      silent = T)

  tryCatch({

    # Prepare study
    antaresEditObject::setPlaylist(playlist = mcyears,opts = opts)
    antaresEditObject::updateGeneralSettings(year.by.year = FALSE, opts = opts)
    antaresEditObject::updateOutputSettings(synthesis = FALSE, storenewset = FALSE, opts=opts)

    for (j in seq_along(list_areas)){
      area <- list_areas[[j]]

      assertthat::assert_that(area %in% names(opts$energyCosts$unserved),
                              msg=paste0("Unserved cost is null in ",area,
                                         ", unserved energy will be exported to this area in simulations launched by the package."))

      suppressWarnings({mingen = antaresRead::readInputTS(mingen = area,opts=opts)})
      if (nrow(mingen)>0){
        assertthat::assert_that(max(dplyr::pull(mingen,"mingen"))==0,
                                msg = paste0("The module is not yet usable with min gen. Please set min gen to zero for area '",area,"'."))
      }

      changeHydroManagement(opts=opts,watervalues = FALSE, heuristic = TRUE, area=area)

      add_fictive_fatal_prod_demand(area = area, opts = opts, load = list_backup[[j]]$load,
                                    misc_gen = list_backup[[j]]$misc_gen)
    }

    # Write grid.csv
    grid = data.frame()
    for (j in seq_along(list_areas)){
      a <- list_areas[[j]]
      grid = data.frame(grid_id = j,
                        problem_name = "all",
                        type = "constraint",
                        name = "HydroPower",
                        area = a,
                        min = 0,
                        max = 1,
                        step = 1/(n_controls-1)) %>%
        rbind(grid)
    }

    body = list()
    out <- character()
    tc <- textConnection("out", "w")
    utils::write.csv(grid, tc, row.names = FALSE, quote = FALSE)
    close(tc)

    body$file <- paste(out, collapse = "\n")
    antaresRead::api_put(opts=opts,endpoint=paste0(opts$study_id,
                                                   "/raw?path=user%2Fwater_values%2Fgrid.csv&create_missing=true&resource_type=file"),
                         body=body)

    # Write penalties.yaml
    params <- list(
      penalty_bottom_rule_curve = penalty_low,
      penalty_upper_rule_curve = penalty_high,
      penalty_final_level = penalty_final_level,
      force_final_level = force_final_level
    )


    body = list()
    tc <- textConnection("out", "w")
    yaml::write_yaml(params, tc)
    close(tc)

    body$file <- out
    antaresRead::api_put(opts=opts,endpoint=paste0(opts$study_id,
                                                   "/raw?path=user%2Fwater_values%2Fpenalties.yaml&create_missing=true&resource_type=file"),
                         body=body)

    # Start the simulations
    res = antaresEditObject::runSimulation(name = name_sim,
                                           opts=opts,
                                           xpansion = list(enabled=T),
                                           other_options = "watervalues")

    assertthat::assert_that(res$status=="success",msg = "Simulation failed.")

    # Extract results
    export_res = antaresRead::api_get(opts = opts,
                                      endpoint = paste0(opts$study_id,
                                                        "/outputs/",res$output_id,
                                                        "/export"))
    max_try <- 5
    i <- 0

    repeat {
      i <- i + 1
      res <- try({download_res = antaresRead::api_get(opts = opts,
                                                      endpoint = export_res$file$id,
                                                      default_endpoint = "v1/downloads")},
                 silent = TRUE)
      if (!inherits(res, "try-error") || i >= max_try) break
      Sys.sleep(10)
    }

    assertthat::assert_that(i<max_try, msg = "Too much attempts to download results.")

    zipfile <- tempfile(fileext = ".zip")
    my_tmpdir <- tempfile("my_zip_files")
    writeBin(download_res, zipfile)
    for (j in seq_along(list_areas)){
      utils::unzip(zipfile, files = paste0("gridPointsValues_",j,".csv"), exdir = my_tmpdir)
      list_watervalues[[list_areas[[j]]]] = utils::read.csv(paste0(my_tmpdir,"/gridPointsValues_",j,".csv"))
    }


    unlink(zipfile)
    on.exit(unlink(my_tmpdir, recursive = TRUE), add = TRUE)
  },
  error = function(e) {
    stop(e)
  },
  finally = {
    for (j in seq_along(list_areas)){
      area = list_areas[[j]]
      restore_fictive_fatal_prod_demand(area = area, opts = opts, load = list_backup[[j]]$load,
                                        misc_gen = list_backup[[j]]$misc_gen)
    }
    antaresEditObject::updateGeneralSettings(year.by.year = year_by_year, opts=opts)
    antaresEditObject::updateOutputSettings(synthesis = synthesis, storenewset = storenewset, opts=opts)
  }
  )

  return(list_watervalues)

}



