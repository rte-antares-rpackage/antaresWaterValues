

#' Calculate Bellman values throughout iterations of Antares simulation and DP
#' Each simulation leads to a new reward estimation, which leads to new water values,
#' which leads to the off-line calculation in R of an optimal trajectory, which leads to
#' new controls to be evaluated which leads to a new simulation
#'
#' @param area Area with the reservoir
#' @param pumping Binary, T if pumping is possible
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
#' @param q_ratio from 0 to 1. the probability used in quantile method
#' to determine a bellman value which q_ratio all bellman values are equal or
#' less to it. (quantile(q_ratio))
#' @param method_fast Method to choose evaluated controls
#' @param test_vu Binary. If you want to run a Antares simulation between each iteration
#' with the latest water values
#' @param force_final_level Binary. Whether final level should be constrained
#' @param final_level_egal_initial Binary. Whether final level, if constrained, should be equal to initial level
#' @param final_level Final level (in percent between 0 and 100) if final level is constrained but different from initial level
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#'
#' @export
#' @return List containing aggregated water values and the data table with all years for the last iteration
calculateBellmanWithIterativeSimulations <- function(area,pumping, pump_eff=1,opts,
                                                     nb_control=10,nb_itr=3,mcyears,
                                                     penalty_low,penalty_high,
                                                     path_solver,study_path,
                                                     states_step_ratio=1/50,
                                                     method_dp = "grid-mean",
                                                     q_ratio = 0.5,
                                                     method_fast = F,
                                                     test_vu=F,
                                                     force_final_level = F,
                                                     final_level_egal_initial = F,
                                                     final_level = NULL,
                                                     penalty_final_level = NULL){


  # Initialization
  df_watervalues <- data.frame()
  df_rewards <- data.frame()
  df_levels <- data.frame()

  max_hydro <- get_max_hydro(area)
  max_hydro_weekly <- max_hydro %>%
    dplyr::mutate(timeId=(.data$timeId-1)%/%168+1) %>%
    dplyr::group_by(.data$timeId) %>%
    dplyr::summarise(pump=sum(.data$pump),turb=sum(.data$turb),.groups = "drop")

  max_hydro <- dplyr::rename(max_hydro,"P_max"="pump","T_max"="turb")

  niveau_max <- get_reservoir_capacity(area = area)

  try(inflow <- antaresRead::readInputTS(hydroStorage = area , timeStep="weekly"),silent = T)
  if (nrow(inflow)==0){
    inflow <- data.table(expand.grid(timeId=1:52,tsId=mcyears,hydroStorage=0,area=area))
  }


  controls <- constraint_generator(area = area,nb_disc_stock = nb_control,
                                   pumping = pumping,opts = opts,
                                   pumping_efficiency = pump_eff,
                                   max_hydro = max_hydro_weekly,inflow = inflow)
  controls <- tidyr::drop_na(controls) %>%
    dplyr::cross_join(data.frame(mcYear=mcyears))

  controls_ref <- controls %>%
    dplyr::rename("u_ref"="u") %>%
    dplyr::select(-c("sim"))

  changeHydroManagement(watervalues = F,heuristic = T,opts = opts,area=area)

  level_init <- get_initial_level(area,opts)*niveau_max/100

  states <- seq(from = niveau_max, to = 0, by = -niveau_max*states_step_ratio)

  reservoir <- readReservoirLevels(area, timeStep = "weekly", byReservoirCapacity = FALSE, opts = opts)

  levels <- getInitialTrend(level_init=level_init,inflow=inflow,mcyears=mcyears,
                  niveau_max=niveau_max,penalty_low=penalty_low,penalty_high=penalty_high,
                  reservoir = reservoir,max_hydro=max_hydro_weekly, states=states,
                  pump_eff = pump_eff)

  i <- 1
  gap <- 1e9
  df_gap <- data.frame()

  while(gap>1&i<=nb_itr){

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
      nb_mcyears = mcyears,
      path_solver =  path_solver,
      binding_constraint = "WeeklyWaterAmount_",
      fictive_area = "fictive_watervalues",
      thermal_cluster = "WaterValueCluster",
      remove_areas = c(),
      overwrite = T,
      link_from = area,
      opts = opts,
      otp_dest = paste0(study_path, "/user"),
      file_name = paste0(i, "_itr_", area),
      pumping = pumping,
      efficiency = pump_eff,
      show_output_on_console = F,
      constraint_values = constraint_values
    )

    controls <- rbind(simulation_res$simulation_values,controls) %>%
      dplyr::select("week","u","mcYear") %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$week,.data$mcYear,.data$u) %>%
      dplyr::mutate(sim="u")

    df_rewards <- updateReward(study_path=study_path,pumping=pumping,
                               controls=controls,max_hydro=max_hydro,mcyears=mcyears,
                               area=area,pump_eff=pump_eff,df_rewards = df_rewards,
                               u0=simulation_res$simulation_values,i=i)

    reward <- df_rewards %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
      dplyr::summarise(reward=min(reward),.groups="drop")

    if (i>1){
      calculated_reward <- getCalculatedReward(reward,levels,expected_reward)

      gap <- mean(abs(calculated_reward$gap))
      df_gap <- dplyr::bind_rows(df_gap,
                                data.frame(gap=gap,n=as.character(i)))


      message(paste0("Actual gap on reward function is : ",gap))

    }

    reward <- reward %>%
      dplyr::left_join(controls_ref,by=c("week","mcYear")) %>%
      dplyr::filter(.data$u==.data$u_ref) %>%
      dplyr::select(-c("u_ref"))

    results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                 mcyears=mcyears,simulation_res=simulation_res,
                                 opts=opts,states_step_ratio=states_step_ratio,
                                 pumping=pumping,pump_eff=pump_eff,
                                 penalty_low=penalty_low,
                                 penalty_high=penalty_high,
                                 inflow=inflow,max_hydro = max_hydro,
                                 max_hydro_weekly = max_hydro_weekly,
                                 niveau_max=niveau_max,
                                 method_dp = method_dp, q_ratio = q_ratio,
                                 force_final_level = force_final_level,
                                 final_level_egal_initial = final_level_egal_initial,
                                 final_level = final_level,
                                 penalty_final_level = penalty_final_level)

    df_watervalues <- dplyr::bind_rows(df_watervalues,
                                       dplyr::mutate(results$aggregated_results,n=as.character(i)))

    levels <- getOptimalTrend(level_init=level_init,watervalues=results$watervalues,
                              mcyears=mcyears,reward=reward,controls=controls,
                              niveau_max = niveau_max,df_levels = df_levels,
                              penalty_low = penalty_low, penalty_high = penalty_high,
                              method_fast = method_fast,
                              max_hydro_weekly=max_hydro_weekly, n=i,
                              pump_eff = pump_eff)

    expected_reward <- getExpectedReward(reward=reward,levels=levels)

    i <- i+1

    if (test_vu){
      wv <- results$aggregated_results
      reshaped_values <- wv[wv$weeks!=53,] %>%
        to_Antares_Format_bis()
      antaresEditObject::writeWaterValues(
        area = area,
        data = reshaped_values
      )

      changeHydroManagement(watervalues = T,heuristic = F,opts = opts,area=area)

      antaresEditObject::runSimulation(
        name = paste0("test_vu_itr_",i),
        mode = "economy",
        path_solver = path_solver,
        show_output_on_console = T,
        opts = opts)

      changeHydroManagement(watervalues = F,heuristic = T,opts = opts,area=area)

    }

  }

  output <- list()
  output$results <- results$aggregated_results
  output$df_rewards <- df_rewards
  output$df_levels <- df_levels
  output$df_watervalues <- df_watervalues
  output$df_gap <- df_gap
  return(output)

}

#' Calculate a trajectory for the reservoir levels inside rule curves
#' taking into account the mean inflow, used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param level_init Initial level of the reservoir in MWh
#' @param inflow Data frame with inflows for each week and each scenario,
#' generated by the function \code{antaresRead::readInputTS}
#' @param mcyears MC years to take into account
#' @param niveau_max Capacity of the reservoir in MWh
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param reservoir Data frame with weekly rule curves with values between 0 and 1,
#' generated by the function \code{readReservoirLevels}
#' @param max_hydro Data frame with weekly maximum pumping and generating power
#' @param states Sequence of possible states
#' @param pump_eff Pumping efficiency (1 if no pumping)
#'
#' @return Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
getInitialTrend <- function(level_init,inflow,mcyears,niveau_max,
                            penalty_low,penalty_high,reservoir,max_hydro,
                            states, pump_eff){

  levels <- data.frame()
  level_i <- level_init
  for (w in 1:52){

    level_high <- reservoir$level_high[[w]]*niveau_max
    level_low <- reservoir$level_low[[w]]*niveau_max

    possible_states <- data.frame(next_state=c(level_low,level_high,states))

    control <- inflow %>%
      dplyr::filter(.data$timeId==w, .data$tsId %in% mcyears) %>%
      dplyr::rename("week"="timeId","mcYear"="tsId") %>%
      dplyr::select(-c("area","time")) %>%
      dplyr::cross_join(possible_states) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(states=level_i,
                    control=min(.data$states+.data$hydroStorage-.data$next_state,max_hydro$turb[w])) %>%
      dplyr::filter(-max_hydro$pump[w]*pump_eff<=.data$control) %>%
      dplyr::group_by(.data$next_state) %>%
      dplyr::filter(dplyr::n()>=length(mcyears)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(penalty_low = dplyr::if_else(.data$next_state<=level_low,penalty_low*(.data$next_state-level_low),0),
                    penalty_high = dplyr::if_else(.data$next_state>=level_high,penalty_high*(level_high-.data$next_state),0),
                    sum=.data$penalty_low+.data$penalty_high) %>%
      dplyr::group_by(.data$next_state) %>%
      dplyr::mutate(sum = mean(.data$sum)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$sum==max(.data$sum)) %>%
      dplyr::mutate(dif_state = abs(.data$next_state-.data$states)) %>%
      dplyr::filter(.data$dif_state==min(.data$dif_state))%>%
      dplyr::rename("lev"="next_state","constraint"="control") %>%
      dplyr::select(c("week","mcYear","lev","constraint")) %>%
      dplyr::distinct(.data$week,.data$mcYear,.keep_all = T)

    assertthat::assert_that(nrow(control)==length(mcyears),msg="Problem with initial trend. Contact the author.")

    levels <- dplyr::bind_rows(levels,control)

    level_i <- unique(control$lev)
  }
  return(levels)
}

#' Update df_rewards with latest simulation run,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param study_path Path of Antares study, passed to \code{setSimulationPath}
#' @param pumping Binary, T if pumping possible
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param max_hydro data.frame {timeId,pump,turb} with maximum pumping and storing
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
#'
#' @return Updated data frame df_rewards
updateReward <- function(study_path,pumping,controls,max_hydro,
                         mcyears,area,pump_eff,u0,df_rewards,i){
  opts_sim <- antaresRead::setSimulationPath(study_path,simulation=-1)

  reward <- get_local_reward(opts=opts_sim,u0=list(u0),
                               possible_controls=controls,max_hydro=max_hydro,
                               mcyears=mcyears,area_price=area,pump_eff= pump_eff)

  reward <- reward_offset(opts=opts_sim,df_reward = reward,
                          u0=list(u0),mcyears=mcyears)

  df_rewards <- dplyr::bind_rows(df_rewards,
                                 dplyr::mutate(reward,n=as.character(i)))
  return(df_rewards)
}

#' Calculate the expected reward based on df_rewards for the new controls to evaluate,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}
#' @param levels Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
#'
#' @return Data frame {mcYear, week, u, reward}
getExpectedReward <- function(reward,levels){
  df_interpolation <- reward %>%
    dplyr::group_by(.data$week,.data$mcYear) %>%
    dplyr::summarise(controls=list(.data$u),
                     rewards=list(reward)) %>%
    dplyr::mutate(f_reward = unlist(mapply(function(x,y)stats::approxfun(x, y),
                                    .data$controls, .data$rewards,
                                    SIMPLIFY = F))) %>%
    dplyr::select(-c("controls","rewards"))

  expected_reward <- df_interpolation %>%
    dplyr::left_join(dplyr::rename(levels,"control"="constraint")) %>%
    dplyr::mutate(reward=mapply(function(y,x)y(x), .data$f_reward, .data$control)) %>%
    dplyr::select(c("mcYear","week","control","reward"))

  return(expected_reward)
}

#' Compare expected reward and Antares calculated reward,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}
#' @param levels Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
#' @param expected_reward Data frame generated by \code{getExpectedReward}
#'
#' @return Data frame with expected reward, calculated reward and gap between them
getCalculatedReward <- function(reward,levels,expected_reward){
  df <- reward %>%
    dplyr::rename("control"="u") %>%
    dplyr::left_join(levels,by=c("week","mcYear")) %>%
    dplyr::filter(.data$constraint==.data$control) %>%
    dplyr::select(c("mcYear","week","control","reward")) %>%
    dplyr::mutate(type="calculated") %>%
    dplyr::bind_rows(dplyr::mutate(expected_reward,type="expected")) %>%
    tidyr::pivot_wider(names_from = "type",values_from = "reward") %>%
    dplyr::mutate(gap=.data$expected-.data$calculated)

  return(df)
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
#' @param pumping Binary, T if pumping is possible
#' @param pump_eff Pumping efficiency between 0 and 1 (1 if no pumping)
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param inflow Data frame with inflows for each week and each scenario,
#' generated by the function \code{antaresRead::readInputTS}
#' @param niveau_max Capacity of the reservoir in MWh
#' @param max_hydro data.frame {timeId,pump,turb} with maximum pumping and storing
#' powers for each hour,returned by the function  \code{get_max_hydro}
#' @param max_hydro_weekly data.frame {timeId,pump,turb} with maximum pumping and storing
#' powers for each week,returned by the function  \code{get_max_hydro}
#' @param method_dp Algorithm in dynamic programming part
#' @param q_ratio from 0 to 1. the probability used in quantile method
#' to determine a bellman value which q_ratio all bellman values are equal or
#' less to it. (quantile(q_ratio))
#' @param force_final_level Binary. Whether final level should be constrained
#' @param final_level_egal_initial Binary. Whether final level, if constrained, should be equal to initial level
#' @param final_level Final level (in percent between 0 and 100) if final level is constrained but different from initial level
#' @param penalty_final_level Penalties (for both bottom and top rule curves) to constrain final level
#'
#' @return List containing aggregated water values and the data table with all years
updateWatervalues <- function(reward,controls,area,mcyears,simulation_res,opts,
                              states_step_ratio,pumping,pump_eff,
                              penalty_low,penalty_high,inflow,niveau_max,
                              max_hydro,max_hydro_weekly, method_dp="grid-mean",
                              q_ratio = 0.5,
                              force_final_level = F,
                              final_level_egal_initial = F,
                              final_level = NULL,
                              penalty_final_level = NULL){

  reward <- dplyr::filter(reward,.data$u==0) %>%
    dplyr::select("mcYear","week","reward") %>%
    dplyr::right_join(reward,by=c("mcYear","week"),suffix=c("_0","")) %>%
    dplyr::mutate(reward=.data$reward-.data$reward_0) %>%
    dplyr::rename("timeId"="week","control"="u") %>%
    dplyr::select(-c("reward_0"))

  reward <- as.data.table(reward)

  reward_db <- list()
  reward_db$reward <- reward
  reward_db$simulation_names <- colnames(reward)[3:length(reward)]
  reward_db$simulation_values <- controls


  results <- Grid_Matrix(
    reward_db = reward_db,
    area = area,
    mcyears = mcyears,
    simulation_names = simulation_res$simulation_names,
    simulation_values = simulation_res$simulation_values,
    nb_cycle = 2,  # cycles to avoid side effect when initialise at 0. Empirically 2 is enough
    opts = opts,
    week_53 = 0,
    district_name = "water values district",
    method = method_dp,
    q_ratio = q_ratio,
    states_step_ratio = states_step_ratio,  # in how many states the reservoirs is divided
    monotonic_bellman = FALSE,  # done in post-process
    inaccessible_states = 99/100,  # for convergence sake
    until_convergence = FALSE,
    convergence_rate = 90/100,  # GUI default value if until_convergence
    convergence_criteria = 1,  # GUI default value if until_convergence
    cycle_limit = 10,  # GUI default value if until_convergence
    pumping = pumping,
    efficiency = pump_eff,
    correct_concavity = F,
    correct_monotony_gain = F,
    penalty_low = penalty_low,
    penalty_high = penalty_high,
    inflow = inflow,
    reservoir_capacity = niveau_max,
    max_hydro_hourly = max_hydro,
    max_hydro_weekly = max_hydro_weekly,
    force_final_level = force_final_level,
    final_level_egal_initial = final_level_egal_initial,
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
#' as \code{getOptimalTrend} and \code{getInitialTrend}
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param method_fast Method to choose evaluated controls
#' @param max_hydro_weekly Data frame with weekly maximum pumping and generating powers
#' @param n Iteration
#' @param pump_eff Pumping efficiency (1 if no pumping)
#'
#' @return Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
getOptimalTrend <- function(level_init,watervalues,mcyears,reward,controls,
                            niveau_max,df_levels,penalty_low,penalty_high,
                            method_fast=F,max_hydro_weekly, n=0, pump_eff){
  level_i <- level_init
  levels <- data.frame()

  states <- watervalues %>%
    dplyr::distinct(.data$states)
  for (w in 1:52){

    if (method_fast){
      state_i <- states %>%
        dplyr::mutate(diff=abs(states-level_i)) %>%
        dplyr::slice(which.min(diff))

      control <- dplyr::filter(watervalues,.data$weeks==w,
                               .data$states==state_i$states) %>%
        dplyr::mutate(lev = min(niveau_max,.data$states+.data$hydroStorage-.data$transition),
                      lev=mean(.data$lev),
                      constraint=level_i+.data$hydroStorage-.data$lev) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(constraint=max(min(.data$constraint,max_hydro_weekly$turb[w]),-max_hydro_weekly$pump[w]*pump_eff)) %>%
        dplyr::rename("week"="weeks","mcYear"="years") %>%
        dplyr::select(c("week","mcYear","lev","constraint"))

      assertthat::assert_that(nrow(control)==length(mcyears),msg="Problem with optimal trend. Contact the author.")


      levels <- dplyr::bind_rows(levels,control)

      level_i <- unique(control$lev)
    } else {

      transition <- watervalues %>%
        dplyr::filter(.data$weeks==dplyr::if_else(w<52,w+1,1))

      # Rule curves at the end of the current week (and beginning of the next one)
      Data_week <- watervalues %>%
        dplyr::filter(.data$weeks==w)
      Data_week$value_node <- NA_real_
      Data_week$transition <- NA_real_
      Data_week$transition_reward <- NA_real_
      Data_week$next_bellman_value <- NA_real_
      Data_week$states <- level_i
      Data_week <- Data_week %>%
        dplyr::distinct(.data$years,.keep_all =T)
      level_high <- Data_week$level_high[1]
      level_low <- Data_week$level_low[1]

      controls_evaluated <- df_levels %>%
        dplyr::filter(.data$week==w) %>%
        dplyr::left_join(Data_week,by=c("mcYear"="years")) %>%
        dplyr::mutate(var=.data$constraint-.data$hydroStorage) %>%
        dplyr::distinct(.data$var) %>%
        dplyr::pull("var")

      # Get interpolation function of rewards for each possible transition for each MC year
      f_reward_year <- get_reward_interpolation(Data_week)
      idx_years <- dplyr::distinct(Data_week[,c('years')]) %>%
        dplyr::pull("years")
      #Get interpolation function of next Bellman values
      df_next_week <- transition %>%
        dplyr::distinct(.data$states,.data$value_node)
      f_next_value <- stats::approxfun(df_next_week$states, df_next_week$value_node)

      # Build a data.table from Data_week that list for each state and each MC year, the possible transitions
      df_next_week <- data.frame(years = transition$years,
                                 next_state = transition$states,
                                 next_value = transition$value_node)

      future_states <- Data_week %>%
        dplyr::inner_join(df_next_week,by="years", relationship="many-to-many") %>%
        dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)

      control_min <- Data_week %>%
        dplyr::mutate(next_state=level_high) %>%
        dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)

      control_max <- Data_week %>%
        dplyr::mutate(next_state=level_low) %>%
        dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)

      df_SDP <- dplyr::bind_rows(future_states, control_min, control_max) %>%
        dplyr::distinct(.data$years,.data$next_state,.keep_all = T) %>%
        dplyr::filter((-max_hydro_weekly$pump[w]*pump_eff<=.data$control)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(distance_evaluated = min(abs(.data$control-.data$hydroStorage-controls_evaluated)),
                      control = min(.data$control,max_hydro_weekly$turb[w])) %>%
        #next_state=dplyr::if_else(.data$states+.data$hydroStorage-.data$control>niveau_max,niveau_max,
        # .data$states+.data$hydroStorage-.data$control)
        dplyr::group_by(.data$next_state) %>%
        dplyr::filter(dplyr::n()>=length(mcyears)) %>%
        dplyr::ungroup()

      df_SDP <- df_SDP %>%
        dplyr::mutate(next_value=mapply(function(x)f_next_value(x), .data$next_state)) %>%
        dplyr::ungroup()


      control <- df_SDP %>%
        dplyr::mutate(gain=mapply(function(y,x)f_reward_year[[which(y==idx_years)]](x), df_SDP$years, df_SDP$control),
                      penalty_low = dplyr::if_else(.data$next_state<=level_low,penalty_low*(.data$next_state-level_low),0),
                      penalty_high = dplyr::if_else(.data$next_state>=level_high,penalty_high*(level_high-.data$next_state),0),
                      sum=.data$gain+.data$next_value+.data$penalty_low+.data$penalty_high) %>%
        dplyr::group_by(.data$next_state) %>%
        dplyr::mutate(sum = mean(.data$sum)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$sum==max(.data$sum)) %>%
        dplyr::filter(.data$distance_evaluated==max(.data$distance_evaluated)) %>%
        dplyr::filter(.data$next_state==max(.data$next_state)) %>%
        dplyr::rename("week"="weeks","mcYear"="years","lev"="next_state","constraint"="control") %>%
        dplyr::select(c("week","mcYear","lev","constraint")) %>%
        dplyr::distinct(.data$week,.data$mcYear,.keep_all = T)

      assertthat::assert_that(nrow(control)==length(mcyears),msg=paste0("Problem with optimal trend at week ",w))

      levels <- dplyr::bind_rows(levels,control)

      level_i <- unique(control$lev)
    }

  }

  return(levels)
}

# getOptimalTrend <- function(level_init,watervalues,mcyears,reward,controls,
#                             niveau_max,df_levels,penalty_low,penalty_high,
#                             method_fast=F,max_hydro_weekly,n=0){
#   levels <- data.frame(week=0,lev=level_init,constraint=0)
#
#   states <- watervalues %>%
#     dplyr::distinct(.data$states)
#
#   df_plot <- data.frame()
#
#   set.seed(n^2)
#
#   for (w in 1:52){
#     level_i <- levels %>%
#       dplyr::filter(.data$week==w-1) %>%
#       dplyr::pull("lev")
#
#     controls_evaluated <- df_levels %>%
#       dplyr::filter(.data$week==w) %>%
#       dplyr::pull("constraint")
#     df_plot <- rbind( df_plot,
#                       data.frame(week=w,control=controls_evaluated)%>%
#                         dplyr::mutate(type="evaluated"))
#
#     # k = sample(mcyears,1)
#     k = mcyears
#
#     if (method_fast){
#       state_i <- states %>%
#         dplyr::mutate(diff=abs(states-level_i)) %>%
#         dplyr::slice(which.min(diff))
#
#       control <- dplyr::filter(watervalues,.data$weeks==w,
#                                .data$states==state_i$states,
#                                .data$years %in% k) %>%
#         dplyr::rowwise() %>%
#         dplyr::mutate(distance_evaluated = min(abs(.data$transition-controls_evaluated))) %>%
#         dplyr::ungroup()
#
#       df_plot <- rbind(df_plot,
#                        dplyr::select(control,c("transition"))%>%
#                          dplyr::rename("control"="transition") %>%
#                          dplyr::mutate(type="possible",week=w))
#
#       control <- control %>%
#         dplyr::slice(which.max(.data$distance_evaluated)) %>%
#         dplyr::rename("inflow"="hydroStorage")
#
#       df_plot <- rbind(df_plot,
#                        dplyr::select(control,c("transition"))%>%
#                          dplyr::rename("control"="transition") %>%
#                          dplyr::mutate(type="chosen",week=w))
#
#       levels <- dplyr::bind_rows(levels,data.frame(week=w,
#                                                    lev=min(niveau_max,level_i-control$transition+control$inflow),
#                                                    constraint=control$transition))
#     } else {
#
#
#
#       transition <- watervalues %>%
#         dplyr::filter(.data$weeks==dplyr::if_else(w<52,w+1,1))
#
#       # Getting all possible transitions between a state for the current week and a state for the next week
#       decision_space <- controls %>%
#         dplyr::filter(.data$week==w) %>%
#         dplyr::pull("u")
#       decision_space <- round(decision_space)
#
#       # Rule curves at the end of the current week (and beginning of the next one)
#       Data_week <- watervalues %>%
#         dplyr::filter(.data$weeks==w,
#                       .data$years %in% k)
#       Data_week$value_node <- NA_real_
#       Data_week$transition <- NA_real_
#       Data_week$transition_reward <- NA_real_
#       Data_week$next_bellman_value <- NA_real_
#       Data_week$states <- level_i
#       Data_week <- Data_week %>%
#         dplyr::distinct(.data$years,.keep_all =T) #%>%
#         # dplyr::mutate(hydroStorage = mean(.data$hydroStorage)) # approximation !
#       level_high <- Data_week$level_high[1]
#       level_low <- Data_week$level_low[1]
#
#       # Possible next states
#       states_next <- Data_week$states_next[[1]]
#       states_next <- unlist(states_next, use.names = FALSE)
#
#       # Get interpolation function of rewards for each possible transition for each MC year
#       f_reward_year <- get_reward_interpolation(Data_week)
#       idx_years <- dplyr::distinct(Data_week[,c('years')]) %>%
#         dplyr::pull("years")
#       #Get interpolation function of next Bellman values
#       df_next_week <- transition %>%
#         dplyr::distinct(.data$states,.data$value_node)
#       f_next_value <- stats::approxfun(df_next_week$states, df_next_week$value_node)
#
#       # Build a data.table from Data_week that list for each state and each MC year, the possible transitions
#       df_next_week <- data.frame(years = transition$years,
#                                  next_state = transition$states,
#                                  next_value = transition$value_node)
#
#       future_states <- Data_week %>%
#         dplyr::inner_join(df_next_week,by="years", relationship="many-to-many") %>%
#         dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)
#
#       control_possible <- Data_week  %>%
#         dplyr::mutate(control=list(decision_space)) %>%
#         tidyr::unnest_longer(.data$control) %>%
#         dplyr::mutate(next_state=.data$states+.data$hydroStorage-.data$control)
#
#       control_min <- Data_week %>%
#         dplyr::mutate(next_state=level_high) %>%
#         dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)
#
#       control_max <- Data_week %>%
#         dplyr::mutate(next_state=level_low) %>%
#         dplyr::mutate(control = -.data$next_state+.data$states+.data$hydroStorage)
#
#       df_SDP <- dplyr::bind_rows(future_states, control_possible, control_min, control_max) %>%
#         dplyr::filter((-max_hydro_weekly$pump[w]<=.data$control)&
#                         (.data$control<=max_hydro_weekly$turb[w])&(.data$next_state>=0))
#       df_SDP <- df_SDP %>%
#         # tidyr::expand(.data$control,.data$years) %>%
#         # dplyr::left_join(dplyr::distinct(df_SDP,.data$states,.data$years,
#         #                                  .data$hydroStorage),by=c("years")) %>%
#         dplyr::mutate(next_state=dplyr::if_else(.data$states+.data$hydroStorage-.data$control>niveau_max,niveau_max,
#                                                 .data$states+.data$hydroStorage-.data$control)) %>%
#         dplyr::mutate(next_value=mapply(function(x)f_next_value(x), .data$next_state)) %>%
#         # dplyr::group_by(.data$control) %>%
#         # dplyr::filter(min(.data$next_state)>=0) %>%
#         dplyr::ungroup()
#
#       # For each transition (control), find the associated reward and for each next state,
#       # calculate penalties for violating rule curves. Then, find for each MC year and each state,
#       # the maximum sum of reward, next bellman value and penalties
#
#       control <- df_SDP %>%
#         dplyr::mutate(gain=mapply(function(y,x)f_reward_year[[which(y==idx_years)]](x), df_SDP$years, df_SDP$control),
#                       penalty_low = dplyr::if_else(.data$next_state<=level_low,penalty_low*(.data$next_state-level_low),0),
#                       penalty_high = dplyr::if_else(.data$next_state>=level_high,penalty_high*(level_high-.data$next_state),0),
#                       sum=.data$gain+.data$next_value+.data$penalty_low+.data$penalty_high) %>%
#         dplyr::group_by(.data$years) %>%
#         dplyr::slice(which.max(.data$sum)) %>%
#         dplyr::mutate(distance_evaluated = min(abs(.data$control-controls_evaluated))) %>%
#         dplyr::ungroup()
#       df_plot <- rbind(df_plot,
#                        dplyr::select(control,c("control"))%>%
#                          dplyr::mutate(type="possible",week=w))
#       control <- control %>%
#         dplyr::slice(which.max(.data$distance_evaluated)) %>%
#         dplyr::rename("inflow"="hydroStorage")
#       df_plot <- rbind(df_plot,
#                        dplyr::select(control,c("control"))%>%
#                          dplyr::mutate(type="chosen",week=w))
#
#       levels <- dplyr::bind_rows(levels,data.frame(week=w,
#                                                    lev=min(niveau_max,level_i-control$control+control$inflow),
#                                                    constraint=control$control))
#     }
#
#   }
#
#   levels <- levels %>%
#     dplyr::filter(.data$week>0)
#
#   p <- ggplot2::ggplot(df_plot) +
#     ggplot2::aes(x = week, y = control, colour = type) +
#     ggplot2::geom_point(shape = "circle", size = 1.5) +
#     ggplot2::scale_color_hue(direction = 1) +
#     ggplot2::coord_flip()
#   print(p)
#
#   return(levels)
# }

