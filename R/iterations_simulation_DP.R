

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
#' @param hours Vector of hours used to evaluate costs/rewards of pumping/generating
#' @param states_step_ratio Discretization ratio to generate steps levels
#' between the reservoir capacity and zero
#'
#' @export
#' @return List containing aggregated water values and the data table with all years for the last iteration
calculateBellmanWithIterativeSimulations <- function(area,pumping, pump_eff=1,opts,
                                                     nb_control=10,nb_itr=3,mcyears,
                                                     penalty_low,penalty_high,
                                                     path_solver,study_path,
                                                     hours=round(seq(0,168,length.out=10)),
                                                     states_step_ratio=1/50){


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
  controls <- tidyr::drop_na(controls)

  changeHydroManagement(watervalues = F,heuristic = T,opts = opts,area=area)

  level_init <- readReservoirLevels(area, timeStep = "daily", byReservoirCapacity = FALSE, opts = opts)[[1,1]]
  level_init <- level_init*niveau_max

  reservoir <- readReservoirLevels(area, timeStep = "weekly", byReservoirCapacity = FALSE, opts = opts)

  levels <- getInitialTrend(level_init=level_init,controls=controls,inflow=inflow,mcyears=mcyears,
                  niveau_max=niveau_max,penalty_low=penalty_low,penalty_high=penalty_high,
                  reservoir = reservoir)

  i <- 1
  gap <- 1e9

  while(gap>1&i<=nb_itr){

    constraint_values <- levels %>%
      dplyr::select(c("week", "constraint")) %>%
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
      show_output_on_console = TRUE,
      constraint_values = constraint_values
    )

    df_rewards <- updateReward(study_path=study_path,pumping=pumping,hours=hours,
                               controls=controls,max_hydro=max_hydro,mcyears=mcyears,
                               area=area,pump_eff=pump_eff,df_rewards = df_rewards,
                               u0=simulation_res$simulation_values$u,i=i)

    reward <- df_rewards %>%
      dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
      dplyr::summarise(reward=min(reward),.groups="drop")

    if (i>1){
      calculated_reward <- getCalculatedReward(reward,levels,expected_reward)

      gap <- mean(calculated_reward$gap)

      message(paste0("Actual gap on reward function is : ",gap))

    }

    results <- updateWatervalues(reward=reward,controls=controls,area=area,
                                 mcyears=mcyears,simulation_res=simulation_res,
                                 opts=opts,states_step_ratio=states_step_ratio,
                                 pumping=pumping,pump_eff=pump_eff,
                                 penalty_low=penalty_low,
                                 penalty_high=penalty_high,
                                 inflow=inflow,max_hydro = max_hydro,
                                 max_hydro_weekly = max_hydro_weekly,
                                 niveau_max=niveau_max)

    df_watervalues <- dplyr::bind_rows(df_watervalues,
                                       dplyr::mutate(results$aggregated_results,n=as.character(i)))

    levels <- getOptimalTrend(level_init=level_init,watervalues=results$watervalues,
                              mcyears=mcyears,reward=reward,controls=controls,
                              niveau_max = niveau_max,df_levels = df_levels,
                              penalty_low = penalty_low, penalty_high = penalty_high)

    expected_reward <- getExpectedReward(reward=reward,levels=levels)

    i <- i+1

  }

  return(results)

}

#' Calculate a trajectory for the reservoir levels inside rule curves
#' taking into account the mean inflow, used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param level_init Initial level of the reservoir in MWh
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param inflow Data frame with inflows for each week and each scenario,
#' generated by the function \code{antaresRead::readInputTS}
#' @param mcyears MC years to take into account
#' @param niveau_max Capacity of the reservoir in MWh
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param reservoir Data frame with weekly rule curves with values between 0 and 1,
#' generated by the function \code{readReservoirLevels}
#'
#' @return Data frame with level (lev), optimal transition (opt) and transition
#' to evaluate (constraint) for each week (w)
getInitialTrend <- function(level_init,controls,inflow,mcyears,niveau_max,
                            penalty_low,penalty_high,reservoir){

  levels <- data.frame(week=0,lev=level_init,opt=0,constraint=0)
  for (w in 1:52){
    level_i <- levels %>% dplyr::filter(week==w-1) %>% dplyr::pull("lev")

    control <- data.frame(states=level_i) %>%
      dplyr::mutate(control=list(dplyr::filter(controls,.data$week==w)$u)) %>%
      tidyr::unnest_longer(control) %>%
      dplyr::cross_join(inflow %>% dplyr::filter(.data$timeId==w, .data$tsId %in% mcyears) %>%
                          dplyr::select(tsId,hydroStorage)) %>%
      dplyr::mutate(next_state=dplyr::if_else(states+hydroStorage-control>niveau_max,niveau_max,
                                states+hydroStorage-control)) %>%
      dplyr::filter(.data$next_state>=0) %>%
      dplyr::cross_join(dplyr::filter(reservoir,.data$timeId==w)) %>%
      dplyr::mutate(penalty_low = dplyr::if_else(.data$next_state<=.data$level_low*niveau_max,penalty_low*(.data$next_state-.data$level_low*niveau_max),0),
             penalty_high = dplyr::if_else(.data$next_state>=.data$level_high*niveau_max,penalty_high*(.data$level_high*niveau_max-.data$next_state),0),
             sum=penalty_low+penalty_high) %>%
      dplyr::group_by(control) %>%
      dplyr::summarise(obj=mean(.data$sum),inflow=mean(.data$hydroStorage),.groups = "drop") %>%
      dplyr::filter(.data$obj==max(.data$obj)) %>%
      dplyr::slice(which.min(abs(.data$control)))

    levels <- dplyr::bind_rows(levels,data.frame(week=w,
                                      lev=level_i-control$control+control$inflow,
                                      opt=control$control,constraint=control$control))
  }
  return(levels)
}

#' Update df_rewards with latest simulation run,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param study_path Path of Antares study, passed to \code{setSimulationPath}
#' @param pumping Binary, T if pumping possible
#' @param hours Vector of hours used to evaluate costs/rewards of pumping/generating,
#' passed to \code{get_local_reward}
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
updateReward <- function(study_path,pumping,hours,controls,max_hydro,
                         mcyears,area,pump_eff,u0,df_rewards,i){
  opts_sim <- antaresRead::setSimulationPath(study_path,simulation=-1)

  if (pumping){
    reward <- get_local_reward(opts=opts_sim,hours=hours,
                               possible_controls=controls,max_hydro=max_hydro,
                               mcyears=mcyears,area_price=area,pump_eff= pump_eff)
  } else {
    reward <- get_local_reward_turb(opts=opts_sim,possible_controls=controls,
                                    area_price=area,mcyears=mcyears,max_hydro = max_hydro)
  }


  reward <- reward_offset(opts=opts_sim,df_reward = reward,
                          u0=u0,mcyears=mcyears)

  df_rewards <- dplyr::bind_rows(df_rewards,
                                 dplyr::mutate(reward,n=as.character(i)))
  return(df_rewards)
}

#' Calculate the expected reward based on df_rewards for the new controls to evaluate,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}
#' @param levels Data frame with level (lev), optimal transition (opt) and transition
#' to evaluate (constraint) for each week (w)
#'
#' @return Data frame {mcYear, week, u, reward}
getExpectedReward <- function(reward,levels){
  expected_reward <- reward %>%
    dplyr::rename("control"="u") %>%
    dplyr::left_join(levels,by=c("week")) %>%
    dplyr::filter(.data$opt==.data$control) %>%
    dplyr::select(-c("constraint","opt","lev"))

  return(expected_reward)
}

#' Compare expected reward and Antares calculated reward,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param reward Data frame containing estimation of the reward function,
#' same format as the output of \code{reward_offset}
#' @param levels Data frame with level (lev), optimal transition (opt) and transition
#' to evaluate (constraint) for each week (w)
#' @param expected_reward Data frame generated by \code{getExpectedReward}
#'
#' @return Data frame with expected reward, calculated reward and gap between them
getCalculatedReward <- function(reward,levels,expected_reward){
  df <- reward %>%
    dplyr::rename("control"="u") %>%
    dplyr::left_join(levels,by=c("week")) %>%
    dplyr::filter(.data$opt==.data$control) %>%
    dplyr::select(-c("constraint","opt","lev")) %>%
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
#'
#' @return List containing aggregated water values and the data table with all years
updateWatervalues <- function(reward,controls,area,mcyears,simulation_res,opts,
                              states_step_ratio,pumping,pump_eff,
                              penalty_low,penalty_high,inflow,niveau_max,
                              max_hydro,max_hydro_weekly){

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
    method = c("grid-mean","mean-grid","quantile")[1], # I want "Grille des moyennes"
    states_step_ratio = states_step_ratio,  # in how many states the reservoirs is divided
    correct_outliers = FALSE,  # if TRUE interpolate to avoid outliers
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
    max_hydro_weekly = max_hydro_weekly
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
#'
#' @return Data frame with level (lev), optimal transition (opt) and transition
#' to evaluate (constraint) for each week (w)
getOptimalTrend <- function(level_init,watervalues,mcyears,reward,controls,
                            niveau_max,df_levels,penalty_low,penalty_high){
  levels <- data.frame(week=0,lev=level_init,opt=0)

  for (w in 1:52){
    level_i <- levels %>%
      dplyr::filter(.data$week==w-1) %>%
      dplyr::pull("lev")

    transition <- watervalues %>%
      dplyr::filter(weeks==dplyr::if_else(w<52,w+1,1))

    f_next_value <- get_bellman_values_interpolation(transition,transition$value_node,mcyears)

    control <- dplyr::filter(watervalues,.data$weeks==w) %>%
      dplyr::mutate(states=level_i) %>%
      dplyr::distinct(.data$years,.data$states,.keep_all = T) %>%
      dplyr::mutate(control=list(dplyr::filter(controls,.data$week==w)$u)) %>%
      tidyr::unnest_longer("control") %>%
      dplyr::mutate(next_state=dplyr::if_else(.data$states+.data$hydroStorage-.data$control>niveau_max,
                                       niveau_max,
                                       .data$states+.data$hydroStorage-.data$control)) %>%
      dplyr::filter(.data$next_state>=0) %>%
      dplyr::mutate(next_value=mapply(function(y,x)f_next_value[[which(y==mcyears)]](x), .data$years, .data$next_state)) %>%
      dplyr::left_join(dplyr::filter(reward,.data$week==w),by=c("years"="mcYear","control"="u")) %>%
      dplyr::mutate(penalty_low = dplyr::if_else(.data$next_state<=.data$level_low,
                                          penalty_low*(.data$next_state-.data$level_low),0),
                    penalty_high = dplyr::if_else(.data$next_state>=.data$level_high,
                                           penalty_high*(.data$level_high-.data$next_state),0),
                    sum=.data$reward+.data$next_value+.data$penalty_low+.data$penalty_high) %>%
      dplyr::group_by(.data$control) %>%
      dplyr::summarise(obj=mean(.data$sum),inflow=mean(.data$hydroStorage),.groups = "drop") %>%
      dplyr::slice(which.max(.data$obj))


    levels <- dplyr::bind_rows(levels,data.frame(week=w,
                                      lev=level_i-control$control+control$inflow,
                                      opt=control$control))

  }

  levels <- levels %>%
    dplyr::filter(.data$week>0) %>%
    dplyr::left_join(controls %>% dplyr::group_by(.data$week) %>% dplyr::summarise(controls = list(.data$u)),
              by=c("week")) %>%
    dplyr::left_join(df_levels %>% dplyr::group_by(.data$week) %>% dplyr::summarise(evaluated = list(.data$constraint)),
              by=c("week")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(possible = list(setdiff(.data$controls,.data$evaluated))) %>%
    dplyr::mutate(constraint = dplyr::if_else((.data$opt %in% .data$possible), .data$opt,
                                .data$possible[which.min(abs(.data$possible - .data$opt))])) %>%
    dplyr::select("week","constraint") %>%
    dplyr::right_join(levels,by=c("week")) %>%
    tidyr::replace_na(list(constraint=0))

  return(levels)
}
