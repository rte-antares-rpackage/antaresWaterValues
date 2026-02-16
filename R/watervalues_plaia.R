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
                                      force_final_level = TRUE,
                                      penalty_final_level = 0,
                                      penalty_low = 0,
                                      penalty_high = 0,
                                      cvar_value = 1,
                                      name_sim = "watervalues",
                                      n_controls = 51) {

  validate_and_normalize_areas(list_areas,opts)

  list_backup = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]
    list_backup[[j]] = getBackupData(area,mcyears,opts)
  }

  list_watervalues = list()

  year_by_year = opts$parameters$general$`year-by-year`
  synthesis = opts$parameters$output$synthesis
  storenewset = opts$parameters$output$storenewset

  validate_api_study_for_plaia(opts)

  df_levels = data.frame()
  df_watervalues = data.frame()

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

      assertthat::assert_that(ncol(list_backup[[j]]$hydro_storage)>=max(mcyears),
                              msg = paste0("There is no enough columns for data inflow for ",area))

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
    out <- utils::capture.output(
      utils::write.csv(grid, row.names = FALSE, quote = FALSE)
    )

    body$file <- paste(out, collapse = "\n")
    antaresRead::api_put(opts=opts,endpoint=paste0(opts$study_id,
                                                   "/raw?path=user%2Fwater_values%2Fgrid.csv&create_missing=true&resource_type=file"),
                         body=body)

    # Write penalties.yaml
    params <- list(
      penalty_bottom_rule_curve = penalty_low,
      penalty_upper_rule_curve = penalty_high,
      penalty_final_level = penalty_final_level,
      force_final_level = force_final_level,
      cvar = cvar_value
    )


    body = list()
    out <- yaml::as.yaml(params)

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
    dir.create(my_tmpdir)
    writeBin(download_res, zipfile)
    for (j in seq_along(list_areas)){
      utils::unzip(zipfile, files = paste0(j,"_",list_areas[[j]],"_water_values.csv"), exdir = my_tmpdir)
      list_watervalues[[list_areas[[j]]]] = as.matrix(utils::read.csv(paste0(my_tmpdir,"/",j,"_",list_areas[[j]],"_water_values.csv"),
                                                            sep = '\t', header = F))
    }

    for (j in seq_along(list_areas)){
      capa = antaresWaterValues::get_reservoir_capacity(list_areas[[j]],opts)
      utils::unzip(zipfile, files = paste0(j,"_",list_areas[[j]],"_bellman_values.csv"), exdir = my_tmpdir)
      data = utils::read.csv(paste0(my_tmpdir,"/",j,"_",list_areas[[j]],"_bellman_values.csv"),
                                                            sep = ' ', header = F)
      bellman = as.data.frame(data[,1:51])
      bellman$weeks <- rownames(bellman)
      df_watervalues <- tidyr::pivot_longer(bellman,
                                    cols = -weeks,
                                    names_to = "statesid",
                                    values_to = "value_node") %>%
        dplyr::mutate(area = list_areas[[j]],
                      states = (as.integer(stringr::str_remove(.data$statesid,"V"))-1)/50*capa,
                      weeks = as.integer(weeks)-2,
                      weeks = dplyr::if_else(weeks==-1,52,weeks)) %>%
        dplyr::select("weeks","states","value_node","area") %>%
        rbind(df_watervalues)

      utils::unzip(zipfile, files = paste0(j,"_",list_areas[[j]],"_optimal_trajectory.csv"), exdir = my_tmpdir)
      data = utils::read.csv(paste0(my_tmpdir,"/",j,"_",list_areas[[j]],"_optimal_trajectory.csv"),
                             sep = ' ', header = F)
      df = as.data.frame(data[,1:ncol(data)-1])
      df$week <- rownames(df)
      df_levels <- tidyr::pivot_longer(df,
                                 cols = -week,
                                 names_to = "mcYear",
                                 values_to = "lev") %>%
        dplyr::mutate(area = list_areas[[j]],
                      n = 0,
                      week = as.integer(week)-1,
                      mcYear = as.integer(stringr::str_remove(.data$mcYear,"V"))) %>%
        dplyr::filter(mcYear %in% mcyears) %>%
        rbind(df_levels)
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

  output <- list()
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$list_watervalues = list_watervalues
  return(output)

}



