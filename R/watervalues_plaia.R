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
  list_watervalues = list()
  df_levels = data.frame()
  df_watervalues = data.frame()

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

  # Write penalties.yaml
  params <- list(
    penalty_bottom_rule_curve = penalty_low,
    penalty_upper_rule_curve = penalty_high,
    penalty_final_level = penalty_final_level,
    force_final_level = force_final_level,
    cvar = cvar_value
  )

  zip_path = prepare_and_launch_plaia(list_areas,
                                      opts,
                                      mcyears,
                                      grid,
                                      params,
                                      name_sim,
                                      "watervalues")

  for (j in seq_along(list_areas)){
    list_watervalues[[list_areas[[j]]]] = as.matrix(extract_from_zip(
      zip_path,
      paste0(j,"_",list_areas[[j]],"_water_values.csv"),
      sep = '\t', header = F)
    )
  }

  for (j in seq_along(list_areas)){
    capa = antaresWaterValues::get_reservoir_capacity(list_areas[[j]],opts)
    data = extract_from_zip(zip_path,
                            paste0(j,"_",list_areas[[j]],"_bellman_values.csv"),
                            sep = ' ', header = F)
    bellman = as.data.frame(data[,1:51])
    bellman$weeks <- rownames(bellman)
    df_watervalues <- tidyr::pivot_longer(bellman,
                                          cols = -.data$weeks,
                                          names_to = "statesid",
                                          values_to = "value_node") %>%
      dplyr::mutate(area = list_areas[[j]],
                    states = (as.integer(stringr::str_remove(.data$statesid,"V"))-1)/50*capa,
                    weeks = as.integer(.data$weeks)-2,
                    weeks = dplyr::if_else(.data$weeks==-1,52,.data$weeks)) %>%
      dplyr::select("weeks","states","value_node","area") %>%
      rbind(df_watervalues)

    data = extract_from_zip(zip_path,
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

  on.exit(unlink(dirname(zip_path), recursive = TRUE), add = TRUE)

  output <- list()
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$list_watervalues = list_watervalues
  return(output)

}



