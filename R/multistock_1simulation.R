#' Compute Bellman values for several storage based on one simulation
#'
#' First, an Antares simulation (based on short-term trajectories or trajectories given by the simulation output of \code{simu}) is run.
#' Marginal prices and costs of this simulation are used to build univariate and independent reward functions for each area with the function \code{get_Reward()} with \code{method_old = F}.
#' Finally, Bellman values and water values are computed with \code{Grid_Matrix()}.
#' This method is fast and gives good results when storage are not too important in the study. Otherwise, it is better to compute Bellman values storage per storage.
#'
#' @inheritParams runWaterValuesSimulationMultiStock
#' @inheritParams Grid_Matrix
#' @param write_vu Binary. True to write water values in the Antares study.
#' @param simu List of simulation parameters returned by the function \code{antaresRead::setSimulationPath()} with the simulation selected from which to use the storage trajectory to run the simulation.
#'
#' @return A \code{dplyr::tibble()} similar to \code{aggregated_results} from \code{Grid_Matrix()} with one additional column \code{"area"}.
#' @export
#'
getBellmanValuesFromOneSimulationMultistock <- function(opts,
                                                        path_solver,
                                                        mcyears,
                                                        list_areas,
                                                        list_pumping,
                                                        list_efficiency,
                                                        penalty_final_level_low,
                                                        penalty_final_level_high,
                                                        penalty_low,
                                                        penalty_high,
                                                        write_vu = F,
                                                        simu = NULL) {
  prefix <- "1sim"

  df_vu <- data.frame()

  constraint_values <- data.frame()

  if (is.null(simu)) {
    for (area in list_areas) {
      max_hydro <- get_max_hydro(area, opts, timeStep = "weekly")
      inflow <- get_inflow(area = area,
                           opts = opts,
                           mcyears = mcyears)

      constraint_values <- inflow %>%
        dplyr::filter(.data$tsId %in% mcyears, .data$timeId <= 52) %>%
        dplyr::left_join(max_hydro, by = dplyr::join_by("timeId")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(hydroStorage = dplyr::if_else(.data$hydroStorage>.data$turb,.data$turb,.data$hydroStorage)) %>%
        dplyr::select(c("timeId", "tsId", "hydroStorage")) %>%
        dplyr::rename("u" = "hydroStorage",
                      "week" = "timeId",
                      "mcYear" = "tsId") %>%
        dplyr::mutate(sim = "u_0") %>%
        dplyr::mutate(area = area) %>%
        dplyr::ungroup() %>%
        rbind(constraint_values)
    }
  } else{
    result_stock <- antaresRead::readAntares(
      areas = list_areas,
      opts = simu,
      select = c("H. STOR", "H. PUMP"),
      timeStep = "weekly",
      mcYears = mcyears
    ) %>%
      dplyr::rename(week = .data$timeId)

    for (a in list_areas) {
      constraint_values <- result_stock %>%
        dplyr::filter(area == a) %>%
        dplyr::mutate(u = .data$`H. STOR` - .data$`H. PUMP` * list_efficiency[a]) %>%
        dplyr::select(c("week", "mcYear", "u")) %>%
        dplyr::mutate(sim = "u_0") %>%
        dplyr::mutate(area = a) %>%
        dplyr::ungroup() %>%
        rbind(constraint_values)
    }

  }

  simulation_res <- runWaterValuesSimulationMultiStock (
    list_areas = list_areas,
    list_pumping = list_pumping,
    list_efficiency = list_efficiency,
    mcyears = mcyears,
    path_solver = path_solver,
    overwrite = T,
    opts = opts,
    file_name = paste0(prefix),
    show_output_on_console = F,
    constraint_values = constraint_values
  )

  for (a in list_areas) {
    pumping <- list_pumping[a]
    pump_eff <- list_efficiency[a]

    print(a)
    local_sim_values <- simulation_res$simulation_values %>%
      dplyr::filter(.data$area == a) %>%
      dplyr::select(-c("area"))

    controls_reward_calculation <- constraint_generator(
      area = a,
      nb_disc_stock = 51,
      pumping = pumping,
      efficiency = pump_eff,
      opts = opts,
      mcyears = mcyears
    )

    reward_db <- get_Reward(
      simulation_values = local_sim_values,
      simulation_names = simulation_res$simulation_names,
      opts = opts,
      area = a,
      mcyears = mcyears,
      method_old = F,
      possible_controls = controls_reward_calculation,
      expansion = simulation_res$expansion,
      efficiency = pump_eff
    )

    results <- Grid_Matrix(
      area = a,
      mcyears = mcyears,
      reward_db = reward_db,
      nb_cycle = 1,
      opts = opts,
      week_53 = 0,
      states_step_ratio = (1 / 51),
      efficiency = pump_eff,
      penalty_low = penalty_low,
      penalty_high = penalty_high,
      penalty_final_level_low = penalty_final_level_low,
      penalty_final_level_high = penalty_final_level_high
    )$aggregated_results


    df_vu <- df_vu %>%
      rbind(dplyr::mutate(results, area = a))

    if (write_vu) {
      reshaped_values <- results[results$weeks != 53, ] %>%
        to_Antares_Format_bis()
      antaresEditObject::writeWaterValues(area = a,
                                          data = reshaped_values)

      changeHydroManagement(
        watervalues = T,
        heuristic = F,
        opts = opts,
        area = a
      )
    }

  }

  return(df_vu)

}
