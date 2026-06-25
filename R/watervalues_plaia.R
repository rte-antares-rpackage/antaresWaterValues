#' Compute Bellman values for several storage with plaia
#'
#' For each storage, reward functions are evaluated and Bellman values are computed with plaia.
#' Results are written directly to the output folder. Final level is equal to initial level for all storages.
#'
#' @inheritParams runWaterValuesSimulationMultiStock
#' @inheritParams Grid_Matrix
#' @param list_force_final_level Named list of logical. Per-area flag indicating whether the final level should be constrained.
#' @param list_penalty_final_level Named list of double. Per-area penalties for both rule curves to constrain final level.
#' @param list_final_level Named list of double. Per-area final reservoir level (in percent between 0 and 100). Used when \code{list_force_final_level} is \code{TRUE}.
#' @param list_penalty_low Named list of double. Per-area penalty for violating the bottom rule curve, comparable to the unsupplied energy cost.
#' @param list_penalty_high Named list of double. Per-area penalty for violating the top rule curve, comparable to the spilled energy cost.
#' @param list_cvar_value Named list of double (values between 0 and 1). Per-area probability used in the CVaR method.
#' @param name_sim Character. Name of the simulation to launch
#' @param n_controls Integer. Number of controls to evaluate reward functions.
#' @param n_levels Integer. Number of Bellman value levels to compute.
#' @param cluster Character. Name of the cluster of antaresWeb
#' @param plaia_path Character. Path to the plaia executable. Required for local studies, ignored for API studies.
#' @param solver Character. Solver used by plaia (e.g. \code{"xpress"}, \code{"coin"}).
#' @param threads Integer. Number of threads used by the plaia executable (local studies only).
#' @return List of watervalues matrix
#' @export
#'
getBellmanValuesWithPlaia <- function(opts,
                                      mcyears,
                                      list_areas,
                                      list_force_final_level = list(),
                                      list_final_level = list(),
                                      list_penalty_final_level  = list(),
                                      list_penalty_low = list(),
                                      list_penalty_high = list(),
                                      list_cvar_value = list(),
                                      name_sim = "watervalues",
                                      n_controls = 51,
                                      n_levels = 101,
                                      cluster = "calin2",
                                      plaia_path = NULL,
                                      solver = "xpress",
                                      threads = 1L) {
  settings <- list(
    solver = solver,
    keep_mps = FALSE,
    problem_format = "OPTIMIZED",
    verbosity = "INFO",
    cache_problems = TRUE
  )
  list_watervalues = list()
  df_levels = data.frame()
  df_watervalues = data.frame()

  # Write grid.csv
  grid = data.frame()
  for (j in seq_along(list_areas)){
    a <- list_areas[[j]]
    max_hydro = get_max_hydro(a, opts, "weekly")
    efficiency = getPumpEfficiency(area = a, opts = opts)
    grid_area = tidyr::expand_grid(mcYear = mcyears, timeId = max_hydro$timeId) %>%
      dplyr::left_join(max_hydro, by = "timeId") %>%
      dplyr::mutate(grid_id = j,
                    problem_name = stringr::str_c("problem-", .data$mcYear, "-", .data$timeId, "--optim-nb-1"),
                    type = "constraint",
                    name = "HydroPower",
                    area = a,
                    min = -.data$pump * efficiency,
                    max = .data$turb,
                    nb_values = n_controls) %>%
      dplyr::select(c("grid_id", "problem_name", "type", "name", "area", "min", "max", "nb_values"))
    grid = rbind(grid, grid_area)
  }

  # penalties.yaml: per-area penalties assembled from per-parameter lists
  penalty_params <- list(
    penalty_bottom_rule_curve = list_penalty_low,
    penalty_upper_rule_curve  = list_penalty_high,
    penalty_final_level       = list_penalty_final_level,
    force_final_level         = list_force_final_level,
    final_level               = list_final_level,
    cvar                      = list_cvar_value
  )
  params <- stats::setNames(
    lapply(list_areas, function(a) {
      area_pen <- lapply(penalty_params, function(p) p[[a]])
      Filter(Negate(is.null), area_pen)
    }),
    list_areas
  )
  params <- c(penalties = list(params), 
    list(start_week = 1L,
    end_week = 52L,
    antares_format = TRUE,
    use_optimal_trajectory = TRUE,
    n_levels = as.integer(n_levels))
  )

  plaia_output_path = prepare_and_launch_plaia(list_areas,
                                      opts,
                                      mcyears,
                                      grid,
                                      params,
                                      name_sim,
                                      "water_values",
                                      cluster,
                                      plaia_path,
                                      settings,
                                      threads)
  on.exit(cleanup_plaia_output(plaia_output_path), add = TRUE)

  for (j in seq_along(list_areas)){
    list_watervalues[[list_areas[[j]]]] = as.matrix(extract_plaia_result(
      plaia_output_path,
      paste0(j,"_",list_areas[[j]],"_water_values.csv"),
      sep = '\t', header = F)
    )
  }

  for (j in seq_along(list_areas)){
    capa = antaresWaterValues::get_reservoir_capacity(list_areas[[j]],opts)
    data = extract_plaia_result(plaia_output_path,
                            paste0(j,"_",list_areas[[j]],"_bellman_values.csv"),
                            sep = ' ', header = F)
    bellman = as.data.frame(data[,1:n_levels])
    bellman$weeks <- rownames(bellman)
    df_watervalues <- tidyr::pivot_longer(bellman,
                                          cols = -.data$weeks,
                                          names_to = "statesid",
                                          values_to = "value_node") %>%
      dplyr::mutate(area = list_areas[[j]],
                    states = (as.integer(stringr::str_remove(.data$statesid,"V"))-1)/(n_levels-1)*capa,
                    weeks = as.integer(.data$weeks)-1) %>%
      dplyr::select("weeks","states","value_node","area") %>%
      rbind(df_watervalues)

    data = extract_plaia_result(plaia_output_path,
                            paste0(j,"_",list_areas[[j]],"_optimal_trajectory.csv"),
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
      dplyr::filter(.data$mcYear %in% mcyears) %>%
      rbind(df_levels)
  }

  output <- list()
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$list_watervalues = list_watervalues
  return(output)

}



