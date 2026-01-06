#' Calculate Bellman values sequentially, one area at a time.
#'
#' For each area, reward functions are first computed using
#' \code{calculateRewardsSimulationsWithPlaia()}. Bellman values are then
#' computed with \code{Grid_Matrix()}, and an optimal trajectory for the area
#' is derived using \code{getOptimalTrend()}.
#' The resulting trajectory is subsequently used to compute reward functions
#' for the next area in the sequence.
#' For areas where Bellman values have not yet been computed, either
#' \code{initial_traj} or a short-term trajectory is used as a fallback.
#'
#' @inheritParams getBellmanValuesFromOneSimulationMultistock
#' @inheritParams Grid_Matrix
#' @param initial_traj Initial trajectory (used for other storages)
#' @param list_areas_to_compute Vector of character. Areas for which to compute Bellman values. If \code{NULL}, all areas in \code{list_areas} are used.
#' @param nb_simulations Number of controls to simulate
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to force final level
#'
#' @export
#' @return List containing aggregated water values, reward functions and optimal trajectories.
getBellmanValuesSequentialMultiStockWithPlaia <- function(list_areas,
                                                          opts,
                                                          nb_simulations,
                                                          mcyears,
                                                          penalty_low,
                                                          penalty_high,
                                                          states_step_ratio=1/50,
                                                          cvar_value = 1,
                                                          penalty_final_level = NULL,
                                                          initial_traj = NULL,
                                                          list_areas_to_compute = NULL){

  # Initialization
  df_watervalues <- data.frame()
  df_rewards <- data.frame()
  df_levels <- data.frame()
  df_lb <- data.frame()

  assertthat::assert_that(opts$antaresVersion>=930,
                          msg = "Plaia is available since version 9.3 of Antares.")

  list_areas = tolower(list_areas)

  list_inflow = list()
  list_capacity = list()
  list_efficiency = list()
  list_max_hydro_weekly = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]
    check_area_name(area = area, opts = opts)

    list_capacity[[j]] <- get_reservoir_capacity(area = area, opts = opts)
    list_inflow[[j]] <- get_inflow(area=area, opts=opts,mcyears=mcyears)
    list_efficiency[[j]] <- getPumpEfficiency(area=area,opts=opts)
    list_max_hydro_weekly[[j]] <- get_max_hydro(area,opts,timeStep = "weekly")
  }

  names(list_efficiency) = list_areas
  names(list_capacity) = list_areas
  names(list_inflow) = list_areas
  names(list_max_hydro_weekly) = list_areas

  if (is.null(initial_traj)){
    initial_traj = data.frame()
    for (j in 1:length(list_areas)){
      initial_traj <- list_inflow[[j]] %>%
        dplyr::filter(.data$tsId %in% mcyears, .data$timeId<=52) %>%
        dplyr::select(c("timeId","tsId","hydroStorage")) %>%
        dplyr::rename("u"="hydroStorage","week"="timeId","mcYear"="tsId") %>%
        dplyr::mutate(area=list_areas[[j]]) %>%
        dplyr::ungroup() %>%
        rbind(initial_traj)

      df_levels = initial_traj %>%
        dplyr::rename(constraint = u) %>%
        dplyr::mutate(lev = NA, scenario = NA, n=0)
    }
  }

  if (is.null(list_areas_to_compute)){
    list_areas_to_compute = list_areas
  }

  for (area in list_areas_to_compute){
    a = area
    final_level <- get_initial_level(area,opts)

    level_init <- get_initial_level_year_per_year(area,opts)*list_capacity[[area]]/100

    reward_db = calculateRewardsSimulationsWithPlaia(area,
                                list_areas,
                                list_efficiency,
                                opts,
                                mcyears,
                                nb_simulations = nb_simulations,
                                initial_traj,
                                list_max_hydro_weekly)

    df_rewards = reward_db$reward %>%
      dplyr::mutate(area = area) %>%
      rbind(df_rewards)

    results <- Grid_Matrix(
      reward_db = reward_db,
      area = area,
      mcyears = mcyears,
      nb_cycle = 1,  # cycles to avoid side effect when initialise at 0. Empirically 2 is enough
      opts = opts,
      week_53 = 0,
      cvar_value = cvar_value,
      states_step_ratio = states_step_ratio,  # in how many states the reservoirs is divided
      efficiency = list_efficiency[[area]],
      penalty_low = penalty_low,
      penalty_high = penalty_high,
      inflow = list_inflow[[area]],
      reservoir_capacity = list_capacity[[area]],
      max_hydro_weekly = list_max_hydro_weekly[[area]],
      force_final_level = T,
      final_level = final_level,
      penalty_final_level_low = penalty_final_level,
      penalty_final_level_high = penalty_final_level
    )

    message(paste0("Lower bound is : ",results$lower_bound))
    df_lb <- dplyr::bind_rows(df_lb,
                                       data.frame(lb=results$lower_bound,
                                                     area=area))

    df_watervalues <- dplyr::bind_rows(df_watervalues,
                                       dplyr::mutate(results$aggregated_results,
                                                     area=area))

    levels <- getOptimalTrend(level_init = level_init,
                              watervalues = results$watervalues,
                              mcyears = mcyears,
                              reservoir_capacity = list_capacity[[area]],
                              penalty_low = penalty_low,
                              penalty_high = penalty_high,
                              penalty_final_level = penalty_final_level,
                              final_level = final_level,
                              max_hydro_weekly =list_max_hydro_weekly[[area]],
                              efficiency = list_efficiency[[area]],
                              mix_scenario = F)

    df_levels = levels %>%
      dplyr::mutate(area = area, n=1) %>%
      rbind(df_levels)

    levels <- levels %>%
      dplyr::mutate(area = area, u = .data$constraint) %>%
      dplyr::select(c("week","mcYear","u","area"))

    initial_traj <- levels %>%
      rbind(dplyr::filter(initial_traj,.data$area!=a))
  }

  output <- list()
  output$df_rewards <- df_rewards
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$lower_bound <- df_lb
  return(output)
}

#' Compute reward function for \code{node} with plaia implementation.
#' The reward function of each week and each year is evaluated on \code{nb_simulations} controls.
#' Called for each area of \code{getBellmanValuesSequentialMultiStockWithPlaia()}.
#' If they are several areas, the trajectories of the other storage are fixed on their \code{optimal_trend}.
#'
#' @param node Character. Name of the area where the reward is computed
#' @inheritParams getBellmanValuesSequentialMultiStockWithPlaia
#' @param optimal_traj Data frame containing optimal trajectory for all areas
#' @param list_max_hydro_weekly List of data.frame. Generated by \code{get_max_hydro()} for each area.
#' @param list_efficiency List of double. Efficiency for each area generated by \code{getPumpEfficiency()}.
#'
#' @returns a \code{data_frame} containing the rewards returned by the function \code{get_Reward()}
calculateRewardsSimulationsWithPlaia <- function(node,
                                        list_areas,
                                        list_efficiency,
                                        opts,
                                        mcyears,
                                        nb_simulations,
                                        optimal_traj,
                                        list_max_hydro_weekly)  {

  list_areas = tolower(list_areas)

  list_backup = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]
    check_area_name(area = area, opts = opts)

    list_backup[[j]] = getBackupData(area,mcyears,opts)

    assertthat::assert_that(ncol(list_backup[[j]]$hydro_storage)>=max(mcyears),
                            msg = paste0("There is no enough columns for data inflow for ",area))
  }

  year_by_year = opts$parameters$general$`year-by-year`
  synthesis = opts$parameters$output$synthesis
  storenewset = opts$parameters$output$storenewset

  assertthat::assert_that(is_api_study(opts),msg="Study must be in API mode to launch simulations.")
  assertthat::assert_that(opts$parameters$`adequacy patch`$`include-adq-patch` == FALSE,
                          msg = "Adequacy Patch can only be used with Economy mode.")

  try(antaresRead::api_post(opts=opts,
                            endpoint = paste0(opts$study_id,"/extensions/xpansion")),
      silent = T)

  tryCatch({

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

    grid = data.frame()
    for (j in seq_along(list_areas)){
      a <- list_areas[[j]]
      if (a != node){
        grid = optimal_traj %>%
          dplyr::filter(.data$area == a) %>%
          dplyr::left_join(list_max_hydro_weekly[[j]],by=c("week"="timeId")) %>%
          dplyr::mutate(rhs = (.data$u+.data$pump*list_efficiency[[j]])/(.data$turb+.data$pump*list_efficiency[[j]])) %>%
          dplyr::mutate(grid_id = 0,
                        problem_name = stringr::str_c("problem-",.data$mcYear,"-",.data$week,"--optim-nb-1"),
                        type = "constraint",
                        name = "HydroPower",
                        min = .data$rhs,
                        max = .data$rhs,
                        step = "0.1") %>%
          dplyr::select(c("grid_id","problem_name","type","name","area","min","max","step")) %>%
          rbind(grid)
      } else {
        grid = data.frame(grid_id = 0,
                          problem_name = "all",
                          type = "constraint",
                          name = "HydroPower",
                          area = a,
                          min = 0,
                          max = 1,
                          step = 1/(nb_simulations-1)) %>%
          rbind(grid)
      }

    }

    body = list()
    out <- utils::capture.output(
      utils::write.csv(grid, row.names = FALSE, quote = FALSE)
    )

    body$file <- paste(out, collapse = "\n")
    antaresRead::api_put(opts=opts,endpoint=paste0(opts$study_id,
                                                   "/raw?path=user%2Fwater_values%2Fgrid.csv&create_missing=true&resource_type=file"),
                         body=body)

    # Start the simulations

    name_sim = paste0("grid_cost_function_",node)

    res = antaresEditObject::runSimulation(name = name_sim,
                                           opts=opts,
                                           xpansion = list(enabled=T),
                                           other_options = "grid_cost_function")

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
    dir.create(my_tmpdir)
    writeBin(download_res, zipfile)
    utils::unzip(zipfile, files = "gridPointsValues_0.csv", exdir = my_tmpdir)

    reward = utils::read.csv(paste0(my_tmpdir,"/gridPointsValues_0.csv"))

    unlink(zipfile)
    on.exit(unlink(my_tmpdir, recursive = TRUE), add = TRUE)

    reward = reward %>%
      dplyr::mutate(timeId = .data$week,
                    mcYear = .data$scenario,
                    control = .data[[paste0(node,"_RHSValue")]],
                    reward = -.data$cost) %>%
      dplyr::select(c("timeId","mcYear","control","reward"))
    assertthat::assert_that(sum(is.na(reward))==0)
    reward_db = list()
    reward_db$reward = reward
    reward_db$decision_space = reward %>%
      dplyr::rename("week"="timeId","u"="control") %>%
      dplyr::select(-c("reward"))
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

  return(reward_db)
}
