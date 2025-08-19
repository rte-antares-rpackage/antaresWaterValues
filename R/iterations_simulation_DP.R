#' Calculate Bellman values throughout iterations of Antares simulation and DP
#' Each simulation leads to a new reward estimation, which leads to new water values,
#' which leads to the off-line calculation in R of an optimal trajectory, which leads to
#' new controls to be evaluated which leads to a new simulation
#'
#' @param area Area with the reservoir
#' @param pumping Binary, TRUE if pumping is possible
#' @param pump_eff Pumping efficiency (1 if no pumping)
#' @param opts  List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param nb_control Number of controls used in the interpolation of the reward function
#' @param nb_itr Max number of iterations
#' @param mcyears Vector of years used to evaluate rewards
#' @param penalty_low Penalty for violating the bottom rule curve, comparable to the unsupplied energy cost
#' @param penalty_high Penalty for violating the top rule curve, comparable to the spilled energy cost
#' @param path_solver Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param study_path Character containing the Antares study
#' @param states_step_ratio Discretization ratio to generate steps levels
#' between the reservoir capacity and zero
#' @param method_dp Algorithm in dynamic programming part
#' @param cvar_value from 0 to 1. the probability used in cvar method
#' @param test_vu Binary. If you want to run a Antares simulation between each iteration
#' with the latest water values
#' @param force_final_level Binary. Whether final level should be constrained
#' @param final_level Final level (in percent between 0 and 100) if final level is constrained but different from initial level
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#' @param initial_traj Initial trajectory
#' @param df_previous_cut Data frame containing previous estimations of cuts
#'
#' @export
#' @return List containing aggregated water values and the data table with all years for the last iteration
calculateBellmanWithIterativeSimulations <- function(area,pumping, pump_eff=1,opts,
                                                     nb_control=10,nb_itr=3,mcyears,
                                                     penalty_low,penalty_high,
                                                     path_solver,study_path,
                                                     states_step_ratio=1/50,
                                                     method_dp = "grid-mean",
                                                     cvar_value = 0.5,
                                                     test_vu=FALSE,
                                                     force_final_level = FALSE,
                                                     final_level = NULL,
                                                     penalty_final_level = NULL,
                                                     initial_traj = NULL,
                                                     df_previous_cut = NULL){


  # Initialization
  df_watervalues <- data.frame()
  df_rewards <- data.frame()
  df_levels <- data.frame()
  df_current_cuts <- data.frame()
  df_gap <- data.frame()
  if (!is.null(df_previous_cut)){
    max_row_per_group = df_previous_cut %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$area,.data$n) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$u),.groups = "drop") %>%
      dplyr::pull("n") %>% max()
    assertthat::assert_that(max_row_per_group==1)
    df_previous_cut <- df_previous_cut %>%
      dplyr::select(c("mcYear","week","u","reward","marg","area","n")) %>%
      dplyr::mutate(n=stringr::str_c("previous_",.data$n))
  }

  max_hydro <- get_max_hydro(area, opts)
  max_hydro_weekly <- max_hydro %>%
    dplyr::mutate(timeId=(.data$timeId-1)%/%168+1) %>%
    dplyr::group_by(.data$timeId) %>%
    dplyr::summarise(pump=sum(.data$pump),turb=sum(.data$turb),.groups = "drop")

  max_hydro <- dplyr::rename(max_hydro,"P_max"="pump","T_max"="turb")

  niveau_max <- get_reservoir_capacity(area = area, opts= opts)

  inflow <- get_inflow(area=area, opts=opts,mcyears=mcyears)

  controls <- constraint_generator(area = area,nb_disc_stock = nb_control,
                                   pumping = pumping,opts = opts,
                                   efficiency = pump_eff,
                                   max_hydro = max_hydro_weekly,inflow = inflow)
  controls <- tidyr::drop_na(controls) %>%
    dplyr::cross_join(data.frame(mcYear=mcyears))

  if (!is.null(df_previous_cut)){
    controls = df_previous_cut %>%
      dplyr::select(c("week","mcYear","u")) %>%
      dplyr::mutate(sim = "u_previous") %>%
      rbind(controls) %>%
      dplyr::left_join(max_hydro_weekly,by=c("week"="timeId")) %>%
      dplyr::filter(-.data$pump*pump_eff<=.data$u, .data$u<= .data$turb) %>%
      dplyr::select(-c("turb","pump")) %>%
      dplyr::arrange(.data$week,.data$mcYear,.data$u,.data$sim) %>%
      dplyr::distinct(.data$week,.data$mcYear,.data$u,.keep_all = TRUE)
    df_rewards = controls %>%
      dplyr::left_join(df_previous_cut, by= dplyr::join_by("mcYear","week"),suffix = c("","_simu")) %>%
      dplyr::mutate(reward = .data$reward +.data$marg * (.data$u-.data$u_simu)) %>%
      dplyr::select(-c("u_simu","marg","sim"))

    reward <- df_rewards %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
      dplyr::summarise(reward=min(reward),.groups="drop")


    results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                 mcyears=mcyears,simulation_res=simulation_res,
                                 opts=opts,states_step_ratio=states_step_ratio,
                                 pump_eff=pump_eff,
                                 penalty_low=penalty_low,
                                 penalty_high=penalty_high,
                                 inflow=inflow,
                                 max_hydro_weekly = max_hydro_weekly,
                                 niveau_max=niveau_max,
                                 method_dp = method_dp, cvar_value = cvar_value,
                                 force_final_level = force_final_level,
                                 final_level = final_level,
                                 penalty_final_level = penalty_final_level)

    message(paste0("Lower bound is : ",results$lower_bound))

    df_gap <- dplyr::bind_rows(df_gap,
                               data.frame(lb=results$lower_bound,n=as.character(0)))

    df_watervalues <- dplyr::bind_rows(df_watervalues,
                                       dplyr::mutate(results$aggregated_results,n=as.character(0)))

  }

  level_init <- get_initial_level(area,opts)*niveau_max/100

  i <- 1
  gap <- 1

  while(gap > 1e-3 && i <= nb_itr){

    tryCatch({

    levels <- getOptimalTrend(level_init=level_init,watervalues=results$watervalues,
                              mcyears=mcyears,reward=reward,controls=controls,
                              niveau_max = niveau_max,df_levels = df_levels,
                              penalty_low = penalty_low, penalty_high = penalty_high,
                              penalty_final_level = penalty_final_level, final_level = final_level,
                              max_hydro_weekly=max_hydro_weekly, n=i,
                              pump_eff = pump_eff,
                              df_previous_cut = df_previous_cut)

    constraint_values <- levels %>%
      dplyr::select(c("week", "constraint","mcYear")) %>%
      dplyr::filter(.data$week>0) %>%
      dplyr::rename("u"="constraint") %>%
      dplyr::mutate(sim="u_1")

    df_levels <- dplyr::bind_rows(df_levels,
                                  dplyr::mutate(levels,n=as.character(i)))

    simulation_res <- runWaterValuesSimulation(
      area = area,
      simulation_name = paste0(i,"_weekly_water_amount_", area, "_%s"),
      nb_disc_stock = 1,
      mcyears = mcyears,
      path_solver =  path_solver,
      overwrite = TRUE,
      opts = opts,
      file_name = paste0(i, "_itr_", area),
      pumping = pumping,
      efficiency = pump_eff,
      show_output_on_console = FALSE,
      constraint_values = constraint_values,
      expansion = TRUE
    )

    if (is_api_study(opts)&opts$antaresVersion>=880){
      output_dir = names(antaresRead::api_get(opts=opts,endpoint=paste0(opts$study_id,"/raw?path=output&depth=1&formatted=false")))
      output_dir = utils::tail(output_dir[stringr::str_detect(output_dir,simulation_res$simulation_names[[1]])],n=1)
      info = antaresRead::api_get(opts=opts,endpoint=paste0(opts$study_id,"/raw?path=output%2F",output_dir,"%2Finfo&depth=2&formatted=false"))
      info$general$mode = "Economy"
      body <- jsonlite::toJSON(info,auto_unbox = TRUE)
      antaresRead::api_post(opts=opts,endpoint=paste0(opts$study_id,
                                                     "/raw?path=output%2F",output_dir,"%2Finfo"),
                           body=body)
    }
    if (!is_api_study(opts)){
      {
        output_dir <- list.dirs(paste0(opts$studyPath,"/output"),recursive = FALSE)
        output_dir = utils::tail(output_dir[stringr::str_detect(output_dir,simulation_res$simulation_names[[1]])],n=1)
        output_info = antaresRead::readIniFile(paste0(output_dir,"/info.antares-output"))
        output_info$general$mode <- "Economy"
        antaresEditObject::writeIniFile(output_info,paste0(output_dir,"/info.antares-output"),
                                        overwrite = TRUE)
      }
    }

    controls <- rbind(simulation_res$simulation_values,controls) %>%
      dplyr::select("week","u","mcYear") %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$week,.data$mcYear,.data$u) %>%
      dplyr::mutate(sim="u")

    o <- updateReward(opts=opts,pumping=pumping,
                               controls=controls,max_hydro_hourly=max_hydro,mcyears=mcyears,
                               area=area,pump_eff=pump_eff,df_rewards = df_rewards,
                               u0=simulation_res$simulation_values,i=i,
                               df_current_cuts = df_current_cuts,
                      df_previous_cut = df_previous_cut)
    df_rewards = o$df_rewards
    df_current_cuts = o$df_current_cuts

    reward <- df_rewards %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
      dplyr::summarise(reward=min(reward),.groups="drop")


    results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                 mcyears=mcyears,simulation_res=simulation_res,
                                 opts=opts,states_step_ratio=states_step_ratio,
                                 pump_eff=pump_eff,
                                 penalty_low=penalty_low,
                                 penalty_high=penalty_high,
                                 inflow=inflow,
                                 max_hydro_weekly = max_hydro_weekly,
                                 niveau_max=niveau_max,
                                 method_dp = method_dp, cvar_value = cvar_value,
                                 force_final_level = force_final_level,
                                 final_level = final_level,
                                 penalty_final_level = penalty_final_level)

    message(paste0("Lower bound is : ",results$lower_bound))

    df_gap <- dplyr::bind_rows(df_gap,
                               data.frame(lb=results$lower_bound,n=as.character(i)))

    if (nrow(df_gap)>=2){
      gap = abs((df_gap$lb[[nrow(df_gap)]]-df_gap$lb[[nrow(df_gap)-1]])/df_gap$lb[[nrow(df_gap)]])


      message(paste0("Actual gap on lower bound is : ",gap*100," %"))

    }

    df_watervalues <- dplyr::bind_rows(df_watervalues,
                                       dplyr::mutate(results$aggregated_results,n=as.character(i)))

    i <- i+1

    if (test_vu){
      wv <- results$aggregated_results
      reshaped_values <- wv[wv$weeks!=53,] %>%
        to_Antares_Format_bis()
      antaresEditObject::writeWaterValues(
        area = area,
        data = reshaped_values
      )

      changeHydroManagement(watervalues = TRUE,heuristic = FALSE,opts = opts,area=area)

      antaresEditObject::runSimulation(
        name = paste0("test_vu_itr_",i),
        mode = "economy",
        path_solver = path_solver,
        show_output_on_console = TRUE,
        opts = opts)

      changeHydroManagement(watervalues = FALSE,heuristic = TRUE,opts = opts,area=area)

    }

    },
    error = function(err) {print(err)}
    )

  }

  output <- list()
  output$results <- results$aggregated_results
  output$df_rewards <- df_rewards
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$df_gap <- df_gap
  output$df_current_cuts <- df_current_cuts
  output$lower_bound <- results$lower_bound
  return(output)

}

#' Update df_rewards with latest simulation run,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param opts Path of Antares study, passed to \code{setSimulationPath}
#' @param pumping Binary, TRUE if pumping possible
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param max_hydro_hourly data.frame \code{timeId,pump,turb} with maximum pumping and storing
#' powers for each hour,returned by the function  \code{get_max_hydro}
#' @param mcyears Vector of monte carlo years used to evaluate rewards
#' @param area Area with the reservoir
#' @param pump_eff Efficient ratio of pumping between 0 and 1
#' @param u0 Constraint values per week used in the simulation,
#' generated by the function \code{constraint_generator}
#' @param df_rewards Data frame containing previous estimations of the reward function,
#' same format as the output of \code{reward_offset} with a column (n) containing the
#' iteration number
#' @param i Iteration number
#' @param df_current_cuts Data frame containing current estimations of cuts
#' @param df_previous_cut Data frame containing previous estimations of cuts
#'
#' @return Updated data frame df_rewards
updateReward <- function(opts,pumping,controls,max_hydro_hourly,
                         mcyears,area,pump_eff,u0,df_rewards,i,df_current_cuts,
                         df_previous_cut){
  if (is_api_study(opts)){
    opts_sim <- antaresRead::setSimulationPathAPI(study_id = opts$study_id,
                                                  host = opts$host,
                                                  token = opts$token,
                                               simulation=paste0(i,"_itr_",area,"_",i,"_weekly_water_amount_",area,"_u_1"))
  } else {
  opts_sim <- antaresRead::setSimulationPath(opts$studyPath,
                                             simulation=paste0(i,"_itr_",area,"_",i,"_weekly_water_amount_",area,"_u_1"))
  }

  u <- u0 %>%
    dplyr::mutate(sim=as.double(stringr::str_extract(.data$sim,"\\d+$"))) %>%
    dplyr::group_by(.data$sim) %>%
    tidyr::nest() %>%
    tidyr::pivot_wider(names_from="sim",values_from="data")

  reward <- get_local_reward(simu=opts_sim,u0=u[[1]][[1]],
                              possible_controls=controls,max_hydro_hourly=max_hydro_hourly,
                               mcyears=mcyears,area=area,efficiency= pump_eff)

  reward <- reward_offset(simu=opts_sim,df_reward = reward,
                          u0=u[[1]][[1]],mcyears=mcyears, expansion = TRUE)

  df_current_cuts = reward %>%
    dplyr::group_by(.data$week,.data$mcYear) %>%
    dplyr::mutate(marg=(.data$reward-dplyr::lag(.data$reward))/(.data$u-dplyr::lag(.data$u)),
                  marg = dplyr::if_else(is.na(.data$marg),dplyr::lead(.data$marg),.data$marg)) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(dplyr::select(u0,-c("sim")),by = dplyr::join_by("mcYear", "week", "u")) %>%
    dplyr::mutate(n=as.character(i),
                  area=area) %>%
    rbind(df_current_cuts)

 if (!is.null(df_previous_cut)){
   a = area
   invalid_previous_cuts = df_current_cuts %>%
     dplyr::filter(.data$n==as.character(i),.data$area == a) %>%
     dplyr::select(-c("marg","n","area")) %>%
     dplyr::right_join(dplyr::filter(df_previous_cut,.data$area==a), by = dplyr::join_by("mcYear", "week"),suffix = c("_current","_previous")) %>%
     dplyr::mutate(reward_estimate = .data$reward_previous + .data$marg * (.data$u_current - .data$u_previous)) %>%
     dplyr::filter(.data$reward_estimate <= .data$reward_current) %>%
     dplyr::select(c("week","mcYear","n","area")) %>%
     dplyr::mutate(to_remove = TRUE)

   df_rewards = df_rewards %>%
     dplyr::left_join(invalid_previous_cuts,by = dplyr::join_by("week", "mcYear", "n", "area")) %>%
     dplyr::filter(is.na(.data$to_remove)) %>%
     dplyr::select(-c("to_remove"))
 }


  reward <- reward %>%
    dplyr::mutate(reward = dplyr::if_else(.data$reward>0,0,.data$reward))

  df_rewards <- dplyr::bind_rows(df_rewards,
                                 dplyr::mutate(reward,n=as.character(i),
                                               area=area))
  output = list()
  output$df_rewards = df_rewards
  output$df_current_cuts = df_current_cuts
  return(output)
}

#' Calculate water values with \code{Grid_Matrix} from estimated reward,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}, not yet offseted with respect to 0
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param area Area with the reservoir
#' @param mcyears Vector of monte carlo years used to evaluate rewards
#' @param simulation_res Generated by the function \code{runWaterValuesSimulation}
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param states_step_ratio Discretization ratio to generate steps levels
#' between the reservoir capacity and zero
#' @param pump_eff Pumping efficiency between 0 and 1 (1 if no pumping)
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param inflow Data frame with inflows for each week and each scenario,
#' generated by the function \code{antaresRead::readInputTS}
#' @param niveau_max Capacity of the reservoir in MWh
#' @param max_hydro_weekly data.frame \code{timeId,pump,turb} with maximum pumping and storing
#' powers for each week,returned by the function  \code{get_max_hydro}
#' @param method_dp Algorithm in dynamic programming part
#' @param cvar_value from 0 to 1. the probability used in quantile method
#' to determine a bellman value which cvar_value all bellman values are equal or
#' less to it. (quantile(cvar_value))
#' @param force_final_level Binary. Whether final level should be constrained
#' @param final_level Final level (in percent between 0 and 100) if final level is constrained but different from initial level
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#'
#' @return List containing aggregated water values and the data table with all years
updateWatervalues <- function(reward,controls,area,mcyears,simulation_res,opts,
                              states_step_ratio,pump_eff,
                              penalty_low,penalty_high,inflow,niveau_max,
                              max_hydro_weekly, method_dp="grid-mean",
                              cvar_value = 0.5,
                              force_final_level = FALSE,
                              final_level = NULL,
                              penalty_final_level = NULL){

  reward <- reward %>%
    dplyr::rename("timeId"="week","control"="u")

  reward <- as.data.table(reward)

  reward_db <- list()
  reward_db$reward <- reward
  reward_db$decision_space <- controls


  results <- Grid_Matrix(
    reward_db = reward_db,
    area = area,
    mcyears = mcyears,
    nb_cycle = 1,  # cycles to avoid side effect when initialise at 0. Empirically 2 is enough
    opts = opts,
    week_53 = 0,
    cvar_value = cvar_value,
    states_step_ratio = states_step_ratio,  # in how many states the reservoirs is divided
    until_convergence = FALSE,
    convergence_rate = 90/100,  # GUI default value if until_convergence
    convergence_criteria = 1,  # GUI default value if until_convergence
    cycle_limit = 10,  # GUI default value if until_convergence
    efficiency = pump_eff,
    correct_concavity = FALSE,
    penalty_low = penalty_low,
    penalty_high = penalty_high,
    inflow = inflow,
    reservoir_capacity = niveau_max,
    max_hydro_weekly = max_hydro_weekly,
    force_final_level = force_final_level,
    final_level = final_level,
    penalty_final_level_low = penalty_final_level,
    penalty_final_level_high = penalty_final_level
  )

  return(results)
}

#' Calculate an optimal trajectory for the reservoir levels based on water values
#' taking into account the mean inflow,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param level_init Initial level of the reservoir in MWh
#' @param watervalues Data frame aggregated watervalues generated by \code{Grid_Matrix}
#' @param mcyears Vector of monte carlo years used to evaluate rewards
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param niveau_max Capacity of the reservoir in MWh
#' @param df_levels Data frame containing all previous evaluated controls, same format
#' as \code{getOptimalTrend}
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param max_hydro_weekly Data frame with weekly maximum pumping and generating powers
#' @param n Iteration
#' @param pump_eff Pumping efficiency (1 if no pumping)
#' @param penalty_final_level Penalty for final level
#' @param final_level Final level
#' @param mix_scenario Should scenario be mix from one week to another ?
#' @param df_previous_cut Data frame containing previous estimations of cuts
#'
#' @return Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
getOptimalTrend <- function(level_init,watervalues,mcyears,reward,controls,
                            niveau_max,df_levels,penalty_low,penalty_high,
                            penalty_final_level, final_level,
                            max_hydro_weekly, n=0, pump_eff,mix_scenario=TRUE,
                            df_previous_cut = NULL){
  level_i <- data.frame(states = level_init,scenario = seq_along(mcyears))
  levels <- data.frame()

  if (is.null(df_previous_cut)){
  if (n==1){
      levels = max_hydro_weekly %>%
        dplyr::mutate(constraint = -.data$pump*pump_eff,
                      true_constraint = .data$constraint,
                      lev = NA) %>%
        dplyr::select(-c("turb","pump")) %>%
        dplyr::rename(week=.data$timeId) %>%
        dplyr::cross_join(data.frame(scenario = seq_along(mcyears),mcYear=mcyears))
      return(levels)
    }

    if (n==2){
    levels = max_hydro_weekly %>%
      dplyr::mutate(constraint = .data$turb,
                    true_constraint = .data$constraint,
                    lev = NA) %>%
      dplyr::select(-c("turb","pump")) %>%
      dplyr::rename(week=.data$timeId) %>%
      dplyr::cross_join(data.frame(scenario = seq_along(mcyears),mcYear=mcyears))
      return(levels)
    }
  }

  set.seed(192 * (n - 1)) # just to make it reproducible

  for (w in 1:52){

    transition <- watervalues %>%
      dplyr::filter(.data$weeks==w+1)

    # Rule curves at the end of the current week (and beginning of the next one)
    Data_week <- watervalues %>%
      dplyr::filter(.data$weeks==w) %>%
      dplyr::filter(.data$statesid == 1)
    Data_week$value_node <- NA_real_
    Data_week$transition <- NA_real_
    Data_week$transition_reward <- NA_real_
    Data_week$next_bellman_value <- NA_real_
    if (mix_scenario){
      Data_week$scenario <- sample(seq_along(mcyears))
    } else {
      Data_week$scenario <- seq_along(mcyears)
    }
    Data_week <- Data_week %>%
      dplyr::select(-c("states")) %>%
      dplyr::left_join(level_i,by=c("scenario"))
    l_high <- ifelse(w<52,Data_week$level_high[1],final_level*niveau_max/100)
    l_low <- ifelse(w<52,Data_week$level_low[1],final_level*niveau_max/100)
    pen_high <- ifelse(w<52,penalty_high,penalty_final_level)
    pen_low <- ifelse(w<52,penalty_low,penalty_final_level)

    # Get interpolation function of rewards for each possible transition for each MC year
    f_reward_year <- get_reward_interpolation(Data_week)

    #Get interpolation function of next Bellman values
    f_next_value <- get_bellman_values_interpolation(transition,transition$value_node,mcyears)

    decision_space <-  dplyr::distinct(Data_week[,c('years','reward_db')]) %>%
      tidyr::unnest(c("reward_db")) %>%
      dplyr::select(c("years","control")) %>%
      dplyr::rename("mcYear"="years","u"="control")

    df_SDP <- build_all_possible_decisions(Data_week,decision_space,f_next_value,
                                           mcyears,l_high,l_low,
                                           max_hydro_weekly$turb[w],
                                           max_hydro_weekly$pump[w]*pump_eff,
                                           transition$value_node,niveau_max,0,
                                           next_states = transition$states)

    control <- df_SDP %>%
      dplyr::mutate(gain=mapply(function(y,x)f_reward_year[[which(y==mcyears)]](x), df_SDP$years, df_SDP$control),
                    penalty_low = dplyr::if_else(.data$next_state<=l_low,pen_low*(.data$next_state-l_low),0),
                    penalty_high = dplyr::if_else(.data$next_state>=l_high,pen_high*(l_high-.data$next_state),0),
                    sum=.data$gain+.data$next_value+.data$penalty_low+.data$penalty_high) %>%
      dplyr::group_by(.data$years) %>%
      dplyr::filter(.data$sum==max(.data$sum)) %>%
      dplyr::filter(.data$next_state==max(.data$next_state)) %>%
      dplyr::ungroup() %>%
      dplyr::rename("week"="weeks","mcYear"="years","lev"="next_state","constraint"="control") %>%
      dplyr::select(c("week","mcYear","lev","constraint","scenario")) %>%
      dplyr::distinct(.data$week,.data$mcYear,.keep_all = TRUE)

    assertthat::assert_that(nrow(control)==length(mcyears),msg=paste0("Problem with optimal trend at week ",w))

    levels <- dplyr::bind_rows(levels,control)

    level_i <- dplyr::select(control,c("lev","scenario")) %>%
      dplyr::rename("states"="lev")

  }

  if (nrow(df_levels)>=1){
  levels <- df_levels %>%
    dplyr::rename(previous_constraint=.data$constraint) %>%
    dplyr::select(c("week","mcYear","previous_constraint")) %>%
    dplyr::left_join(levels,by = dplyr::join_by("week", "mcYear")) %>%
    dplyr::mutate(dis = abs(.data$constraint-.data$previous_constraint)) %>%
    dplyr::left_join(max_hydro_weekly, by=c("week"="timeId")) %>%
    dplyr::group_by(.data$week,.data$mcYear) %>%
    dplyr::summarise(lev = min(.data$lev), scenario = min(.data$scenario), true_constraint = min(.data$constraint),
                     constraint = dplyr::if_else(min(.data$dis)<=min(.data$turb+.data$pump*pump_eff)/1000,
                                                 stats::runif(1,min(-.data$pump*pump_eff),min(.data$turb)),
                                                   min(.data$constraint)),.groups = "drop") %>%
    dplyr::ungroup()
  } else {
    levels <- levels %>%
      dplyr::mutate(true_constraint = .data$constraint)
  }

  return(levels)
}

#' Calculate Bellman values throughout iterations of Antares simulation and DP
#' Each simulation leads to a new reward estimation, which leads to new water values,
#' which leads to the off-line calculation in R of an optimal trajectory, which leads to
#' new controls to be evaluated which leads to a new simulation
#'
#' @param opts  List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param nb_control Number of controls used in the interpolation of the reward function
#' @param nb_itr Max number of iterations
#' @param mcyears Vector of years used to evaluate rewards
#' @param penalty_low Penalty for violating the bottom rule curve, comparable to the unsupplied energy cost
#' @param penalty_high Penalty for violating the top rule curve, comparable to the spilled energy cost
#' @param path_solver Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param study_path Character containing the Antares study
#' @param states_step_ratio Discretization ratio to generate steps levels
#' between the reservoir capacity and zero
#' @param method_dp Algorithm in dynamic programming part
#' @param cvar_value from 0 to 1. the probability used in quantile method
#' to determine a bellman value which cvar_value all bellman values are equal or
#' less to it. (quantile(cvar_value))
#' @param test_vu Binary. If you want to run a Antares simulation between each iteration
#' with the latest water values
#' @param force_final_level Binary. Whether final level should be constrained
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#' @inheritParams runWaterValuesSimulationMultiStock
#' @inheritParams calculateBellmanWithIterativeSimulations
#'
#' @export
#' @return List containing aggregated water values and the data table with all years for the last iteration
calculateBellmanWithIterativeSimulationsMultiStock <- function(list_areas,list_pumping, list_efficiency,opts,
                                                               nb_control=10,nb_itr=3,mcyears,
                                                               penalty_low,penalty_high,
                                                               path_solver,study_path,
                                                               states_step_ratio=1/50,
                                                               method_dp = "grid-mean",
                                                               cvar_value = 0.5,
                                                               test_vu=FALSE,
                                                               force_final_level = FALSE,
                                                               penalty_final_level = NULL,
                                                               initial_traj = NULL,
                                                               df_previous_cut = NULL){

  # Initialization
  df_watervalues <- data.frame()
  df_rewards <- data.frame()
  df_levels <- data.frame()
  df_gap <- data.frame()
  df_current_cuts <- data.frame()
  if (!is.null(df_previous_cut)){
    max_row_per_group = df_previous_cut %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$area,.data$n) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$u),.groups = "drop") %>%
      dplyr::pull("n") %>% max()
    assertthat::assert_that(max_row_per_group==1)
    df_previous_cut <- df_previous_cut %>%
      dplyr::select(c("mcYear","week","u","reward","marg","area","n")) %>%
      dplyr::mutate(n=stringr::str_c("previous_",.data$n))
  }

  for (area in list_areas){
    a = area
    pumping <- list_pumping[area]
    pump_eff <- list_efficiency[area]
    final_level <- get_initial_level(area,opts)

    max_hydro <- get_max_hydro(area, opts)
    max_hydro_weekly <- max_hydro %>%
      dplyr::mutate(timeId=(.data$timeId-1)%/%168+1) %>%
      dplyr::group_by(.data$timeId) %>%
      dplyr::summarise(pump=sum(.data$pump),turb=sum(.data$turb),.groups = "drop")

    max_hydro <- dplyr::rename(max_hydro,"P_max"="pump","T_max"="turb")

    niveau_max <- get_reservoir_capacity(area = area, opts = opts)

    inflow <- get_inflow(area=area, opts=opts,mcyears=mcyears)

    controls <- constraint_generator(area = area,nb_disc_stock = nb_control,
                                     pumping = pumping,opts = opts,
                                     efficiency = pump_eff,
                                     max_hydro = max_hydro_weekly,inflow = inflow)
    controls <- tidyr::drop_na(controls) %>%
      dplyr::cross_join(data.frame(mcYear=mcyears))

    changeHydroManagement(watervalues = FALSE,heuristic = TRUE,opts = opts,area=area)

    if (!is.null(df_previous_cut)){
      controls = df_previous_cut %>%
        dplyr::filter(.data$area == a) %>%
        dplyr::select(c("week","mcYear","u")) %>%
        dplyr::mutate(sim = "u_previous") %>%
        rbind(controls) %>%
        dplyr::left_join(max_hydro_weekly,by=c("week"="timeId")) %>%
        dplyr::filter(-.data$pump*pump_eff<=.data$u, .data$u<= .data$turb) %>%
        dplyr::select(-c("turb","pump")) %>%
        dplyr::arrange(.data$week,.data$mcYear,.data$u,.data$sim) %>%
        dplyr::distinct(.data$week,.data$mcYear,.data$u,.keep_all = TRUE)
      df_rewards = controls %>%
        dplyr::left_join(dplyr::filter(df_previous_cut,.data$area == a), by= dplyr::join_by("mcYear","week"),suffix = c("","_simu")) %>%
        dplyr::mutate(reward = .data$reward + .data$marg * (.data$u-.data$u_simu)) %>%
        dplyr::select(-c("u_simu","marg","sim")) %>%
        rbind(df_rewards)

      reward <- df_rewards %>%
        dplyr::filter(.data$area == a) %>%
        dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
        dplyr::summarise(reward=min(reward),.groups="drop")


      results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                  mcyears=mcyears,simulation_res=simulation_res,
                                  opts=opts,states_step_ratio=states_step_ratio,
                                  pump_eff=pump_eff,
                                  penalty_low=penalty_low,
                                  penalty_high=penalty_high,
                                  inflow=inflow,
                                  max_hydro_weekly = max_hydro_weekly,
                                  niveau_max=niveau_max,
                                  method_dp = method_dp, cvar_value = cvar_value,
                                  force_final_level = force_final_level,
                                  final_level = final_level,
                                  penalty_final_level = penalty_final_level)

      message(paste0("Lower bound is : ",results$lower_bound))

      df_gap <- dplyr::bind_rows(df_gap,
                                data.frame(lb=results$lower_bound,n=as.character(0),area=area))

      df_watervalues <- dplyr::bind_rows(df_watervalues,
                                        dplyr::mutate(results$aggregated_results,n=as.character(0),area=area))

  }

    level_init <- get_initial_level(area,opts)*niveau_max/100

    i <- 1
    gap <- 1

    while(gap>1e-3 && i<=nb_itr){

      tryCatch({

        if (nrow(df_levels)>=1){
          df_levels_area = dplyr::filter(df_levels,.data$area==a)
        } else {
          df_levels_area = df_levels
        }

        levels <- getOptimalTrend(level_init=level_init,watervalues=results$watervalues,
                                  mcyears=mcyears,reward=reward,controls=controls,
                                  niveau_max = niveau_max,df_levels = df_levels_area,
                                  penalty_low = penalty_low, penalty_high = penalty_high,
                                  penalty_final_level = penalty_final_level, final_level = final_level,
                                  max_hydro_weekly=max_hydro_weekly, n=i,
                                  pump_eff = pump_eff, df_previous_cut = df_previous_cut)

        constraint_values <- levels %>%
          dplyr::select(c("week", "constraint","mcYear")) %>%
          dplyr::filter(.data$week>0) %>%
          dplyr::rename("u"="constraint") %>%
          dplyr::mutate(area = area) %>%
          rbind(dplyr::filter(initial_traj,.data$area!=a)) %>%
          dplyr::mutate(sim="u_1")

        df_levels <- dplyr::bind_rows(df_levels,
                                      dplyr::mutate(levels,n=as.character(i),area=area))

        if (TRUE){
          simulation_res <- runWaterValuesSimulationMultiStock(
            list_areas = list_areas,
            list_pumping = list_pumping,
            list_efficiency = list_efficiency,
            simulation_name = paste0(i,"_weekly_water_amount_", area, "_%s"),
            mcyears = mcyears,
            path_solver =  path_solver,
            overwrite = TRUE,
            opts = opts,
            file_name = paste0(i, "_itr_", area),
            constraint_values = constraint_values,
            expansion = TRUE
          )
        } else {
          load(paste0(study_path,"/user/",paste0(i, "_itr_", area),".RData"))
        }

        if (is_api_study(opts)&opts$antaresVersion>=880){
          output_dir = names(antaresRead::api_get(opts=opts,endpoint=paste0(opts$study_id,"/raw?path=output&depth=1&formatted=false")))
          output_dir = utils::tail(output_dir[stringr::str_detect(output_dir,simulation_res$simulation_names[[1]])],n=1)
          info = antaresRead::api_get(opts=opts,endpoint=paste0(opts$study_id,"/raw?path=output%2F",output_dir,"%2Finfo&depth=2&formatted=false"))
          info$general$mode = "Economy"
          body <- jsonlite::toJSON(info,auto_unbox = TRUE)
          antaresRead::api_post(opts=opts,endpoint=paste0(opts$study_id,
                                                          "/raw?path=output%2F",output_dir,"%2Finfo"),
                                body=body)
        }
        if (!is_api_study(opts)){
          {
            output_dir <- list.dirs(paste0(opts$studyPath,"/output"),recursive = FALSE)
            output_dir = utils::tail(output_dir[stringr::str_detect(output_dir,simulation_res$simulation_names[[1]])],n=1)
            output_info = antaresRead::readIniFile(paste0(output_dir,"/info.antares-output"))
            output_info$general$mode <- "Economy"
            antaresEditObject::writeIniFile(output_info,paste0(output_dir,"/info.antares-output"),
                                            overwrite = TRUE)
          }
        }


        controls <- simulation_res$simulation_values %>%
          dplyr::filter(.data$area==a)  %>%
          dplyr::select(-c(area)) %>%
          rbind(controls) %>%
          dplyr::select("week","u","mcYear") %>%
          dplyr::distinct() %>%
          dplyr::arrange(.data$week,.data$mcYear,.data$u) %>%
          dplyr::mutate(sim="u")

        o <- updateReward(opts=opts,pumping=pumping,
                                   controls=controls,max_hydro_hourly=max_hydro,mcyears=mcyears,
                                   area=area,pump_eff=pump_eff,df_rewards = df_rewards,
                         u0=dplyr::filter(simulation_res$simulation_values,.data$area==a),i=i,
                         df_current_cuts = df_current_cuts,
                         df_previous_cut = df_previous_cut)
        df_rewards = o$df_rewards
        df_current_cuts = o$df_current_cuts

        reward <- df_rewards %>%
          dplyr::filter(.data$area == a) %>%
          dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
          dplyr::summarise(reward=min(reward),.groups="drop")


        results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                     mcyears=mcyears,simulation_res=simulation_res,
                                     opts=opts,states_step_ratio=states_step_ratio,
                                     pump_eff=pump_eff,
                                     penalty_low=penalty_low,
                                     penalty_high=penalty_high,
                                     inflow=inflow,
                                     max_hydro_weekly = max_hydro_weekly,
                                     niveau_max=niveau_max,
                                     method_dp = method_dp, cvar_value = cvar_value,
                                     force_final_level = force_final_level,
                                     final_level = final_level,
                                     penalty_final_level = penalty_final_level)

        message(paste0("Lower bound is : ",results$lower_bound))

        df_gap <- dplyr::bind_rows(df_gap,
                                   data.frame(lb=results$lower_bound,n=as.character(i),area=area))

        if (nrow(dplyr::filter(df_gap,.data$area==a))>=2){
          gap = abs(df_gap$lb[[nrow(df_gap)]]-df_gap$lb[[nrow(df_gap)-1]])/df_gap$lb[[nrow(df_gap)]]


          message(paste0("Actual gap on lower bound is : ",gap*100," %"))

        }

        df_watervalues <- dplyr::bind_rows(df_watervalues,
                                           dplyr::mutate(results$aggregated_results,n=as.character(i),area=area))

        i <- i+1

      },
      error = function(err) {
        print(err)
      },
      finally = {
      })
    }

    levels <- getOptimalTrend(level_init=level_init,watervalues=results$watervalues,
                              mcyears=mcyears,reward=reward,controls=controls,
                              niveau_max = niveau_max,df_levels = dplyr::filter(df_levels,.data$area==a),
                              penalty_low = penalty_low, penalty_high = penalty_high,
                              penalty_final_level = penalty_final_level, final_level = final_level,
                              max_hydro_weekly=max_hydro_weekly, n=i,
                              pump_eff = pump_eff, mix_scenario = FALSE)

    df_levels <- dplyr::bind_rows(df_levels,
                                  dplyr::mutate(levels,n=as.character(i),area=area))

    initial_traj <- levels %>%
      dplyr::select(c("week", "true_constraint","mcYear")) %>%
      dplyr::filter(.data$week>0) %>%
      dplyr::rename("u"="true_constraint") %>%
      dplyr::mutate(area = area) %>%
      rbind(dplyr::filter(initial_traj,.data$area!=a))

  }

  output <- list()
  output$df_rewards <- df_rewards
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$df_gap <- df_gap
  output$df_current_cuts <- df_current_cuts
  return(output)

}
