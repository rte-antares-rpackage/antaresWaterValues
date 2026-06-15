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
#' @param cluster Character. Name of the cluster of antaresWeb
#' @param plaia_path Character. Path to the plaia executable. Required for local studies, ignored for API studies.
#' @param settings List. Additional settings passed to plaia via \code{settings.yaml}.
#' @param threads Integer. Number of threads used by the plaia executable (local studies only).
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
                                                          list_final_level = NULL,
                                                          initial_traj = NULL,
                                                          list_areas_to_compute = NULL,
                                                          cluster = "calin1",
                                                          plaia_path = NULL,
                                                          settings = list(),
                                                          threads = 1L){

  # Initialization
  df_watervalues <- data.frame()
  df_rewards <- data.frame()
  df_levels <- data.frame()
  df_lb <- data.frame()

  assertthat::assert_that(opts$antaresVersion>=930,
                          msg = "Plaia is available since version 9.3 of Antares.")

  validate_and_normalize_areas(list_areas,opts)

  list_inflow = list()
  list_capacity = list()
  list_efficiency = list()
  list_max_hydro_weekly = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]

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
        dplyr::rename(constraint = .data$u) %>%
        dplyr::mutate(lev = NA, scenario = NA, n=0)
    }
  }

  if (is.null(list_areas_to_compute)){
    list_areas_to_compute = list_areas
  }

  for (area in list_areas_to_compute){
    a = area
    if (!is.null(list_final_level)){
      final_level <- list_final_level[[a]]
    } else {
      final_level <- get_initial_level(a, opts)
    }

    level_init <- get_initial_level_year_per_year(area,opts)*list_capacity[[area]]/100

    reward_db = calculateRewardsSimulationsWithPlaia(area,
                                list_areas,
                                list_efficiency,
                                opts,
                                mcyears,
                                nb_simulations = nb_simulations,
                                initial_traj,
                                list_max_hydro_weekly,
                                cluster,
                                plaia_path,
                                settings,
                                threads)

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
#' @param cluster Character. Name of the cluster of antaresWeb
#'
#' @returns a \code{data_frame} containing the rewards returned by the function \code{get_Reward()}
calculateRewardsSimulationsWithPlaia <- function(node,
                                        list_areas,
                                        list_efficiency,
                                        opts,
                                        mcyears,
                                        nb_simulations,
                                        optimal_traj,
                                        list_max_hydro_weekly,
                                        cluster = "calin1",
                                        plaia_path = NULL,
                                        settings = list(),
                                        threads = 1L)  {

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

  plaia_output_path = prepare_and_launch_plaia(list_areas,
                                      opts,
                                      mcyears,
                                      grid,
                                      name_sim = paste0("grid_cost_function_",node),
                                      other_options = "grid_evaluator",
                                      cluster = cluster,
                                      plaia_path = plaia_path,
                                      settings = settings,
                                      threads = threads)

  reward = extract_plaia_result(plaia_output_path,
                            "gridPointsValues_0.csv")

  if (grepl("\\.zip$", plaia_output_path)) {
    on.exit(unlink(dirname(plaia_output_path), recursive = TRUE), add = TRUE)
  }

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

  return(reward_db)
}
