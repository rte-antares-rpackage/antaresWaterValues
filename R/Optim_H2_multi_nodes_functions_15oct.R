#' Compute optimal candidates for H2 system
#' 
#' Search optimal solution for bounded candidates for the H2 systems including storage, must-run clusters and flexibles clusters.
#' 
#' @param areas_invest Vector of characters of the names of areas to optimize.
#' @param max_ite Integer. Maximum number of iterations for each area.
#' @param storage_bounds Vector of integers of length 2, with the form (min, max).
#' @param storage_points_nb Integer. Number of storage points to test at each iteration. 
#' Must be >3 to update bounds at each iterations and approach solution.
#' @param candidates_data List of vector of doubles of length 3. One vector of double for each cluster candidate.
#' The vectors of doubles have the form (bound min, bound max, number of points).
#' The number of points must be >3 to update bounds at each iterations and approach solution.
#' @param candidates_types Data_frame with column names : c("index", "name", "type", "TOTEX", "Marg_price").
#' Each row describes a cluster candidate. The index should correspond to the index of the candidate in \code{candidates_data}.
#' The name is a character, the type is either "cluster_flexible" or "cluster_bande", TOTEX is in eur/MW/year, Marg_price is in eur/MWh
#' @param pumping Boolean. True to take into account the pumping capacity.
#' @param penalty_low Integer. Penalty for lower guide curve.
#' @param penalty_high Integer. Penalty for higher guide curve.
#' @param penalty_final_level Integer. Penalty for higher and lower final level.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param mc_years_optim Vector of integers. Monte Carlo years to perform the optimization.
#' @param path_to_antares Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param study_path Character. Path to the simulation, argument passed to \code{antaresRead::setSimulationPath}.
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param storage_annual_cost Numeric. Annual cost of storage in eur/MWh.
#' @param launch_sims Boolean. True to launch simulations at each iterations. False if simulations already run.
#' @param storage_final_level Numeric in [0, 100]. Final and initial level in % of H2 storage.
#' @param step_number Integer. Step number for reward functions. If \code{launch_sims==F}, must be in adequacy with previous step number.
#' @param parallelprocess Boolean. True to compute Water values with parallel processing.
#' @param nb_sockets Integer. Number of sockets for parallel processing
#' @param unspil_cost Numeric. Unspilled energy cost in eur/MWh.
#' @param edit_study Boolean. True to edit study with optimal candidates.
#' 
#' @returns a \code{list} containing for each area detailed results (best candidate, all total costs, reward function, optimization time)
MultiStock_H2_Investment_reward_compute_once <- function(areas_invest,
                                                         max_ite,
                                                         storage_bounds,
                                                         storage_points_nb,
                                                         candidates_data,
                                                         candidates_types,
                                                         pumping=F,
                                                         penalty_low=5000,
                                                         penalty_high=5000,
                                                         penalty_final_level=5000,
                                                         opts, 
                                                         mc_years_optim,
                                                         path_to_antares,
                                                         study_path,
                                                         cvar=1,
                                                         storage_annual_cost,
                                                         launch_sims=T,
                                                         storage_final_level = 60,
                                                         step_number = 51,
                                                         parallelprocess = F,
                                                         nb_sockets = 0,
                                                         unspil_cost = 3000,
                                                         edit_study = F) {
  
  # initialization
  output_node <- list()
  storage_bounds_init <- storage_bounds
  candidates_data_init <- candidates_data
  nb_node <- 0
  
  # edit unsupplied energy cost
  df_econ_options <- data.frame(areas_invest, c(seq(unspil_cost, unspil_cost, length.out = length(areas_invest))))
  colnames(df_econ_options) <- c("area", "average_unsupplied_energy_cost")
  antaresEditObject::writeEconomicOptions(df_econ_options, opts)
  
  # main loop on areas
  for (node in areas_invest) {
    time1 <- Sys.time()
    criteria <- F
    nb_ite <- 0
    nb_node <- nb_node +1
    
    pump_eff <- antaresWaterValues::getPumpEfficiency(node,opts=opts)
    final_level <-antaresWaterValues::get_initial_level(node,opts)
    
    grid_costs <- data.frame(matrix(ncol = 2+length(candidates_types$index), nrow=0))
    
    storage_bounds <- storage_bounds_init
    candidates_data <- candidates_data_init
    
    # getting reward functions
    list_rewards <- list()
    
    antaresEditObject::writeIniHydro(area = node, params = c("reservoir capacity" = storage_bounds[2]), opts = opts)
    simulation_point_res <- 
      calculateRewards5Simulations(area=node,
                                   opts=opts,
                                   pumping = pumping,
                                   pump_eff = pump_eff,
                                   mcyears = mc_years_optim,
                                   path_to_antares =  path_to_antares,
                                   study_path = study_path,
                                   prefix=paste0("unsp", as.character(unspil_cost), "_", nb_node),
                                   launch_sims=launch_sims)
    
    # store results edited by area index
    list_rewards[[as.character(nb_node)]] <- simulation_point_res

    # loop on the candidates grids
    while (criteria | (nb_ite < max_ite)) {
      nb_ite <- nb_ite + 1
      
      # compute storage volumes to evaluate
      if (nb_ite>1) {
        old_storage_points <- new_storage_points
      } else {old_storage_points <-  c()}
      new_storage_points <- c(seq(storage_bounds[1], storage_bounds[2], length.out = storage_points_nb))
      for (sto in 1:length(new_storage_points)) {new_storage_points[sto] <- as.integer(new_storage_points[sto])}
      
      # compute grid of the other candidates
      second_candidate_grid <- grid_other_candidates(candidates_data)
      new_candidate_grid <- second_candidate_grid
      if (nb_ite > 1) {
        for (can in 1:length(second_candidate_grid)) {
          for (can2 in 1:length(candidate_grid)) {
            if (all(second_candidate_grid[[can]] == candidate_grid[[can2]]) & (storage_vol %in% old_storage_points)) {
              new_candidate_grid[[can]] <- "not necessary"
            }
          }
        }
      }
      candidate_grid <- second_candidate_grid
      
      # loop on the storage candidates
      for (storage_vol in new_storage_points) {
        
        if (parallelprocess == T) {
          # with parallel processing
          # create clusters
          cl <- parallel::makeCluster(nb_sockets)
          parallel::clusterExport(cl, c("total_cost_parallel_version", 
                                        "total_cost_loop", 
                                        "update_reward_cluster_bande",
                                        "update_reward_cluster_flexible",
                                        "storage_annual_cost"),
                                  envir=environment())
          
          
          list_index <- c(seq(1, length(new_candidate_grid)))
          print("begin parallel processing")
          print(storage_vol)
          # compute WV with parallel processing
          new_grid_costs <- parallel::parLapply(cl, list_index, function(x) {
            opts <- antaresRead::setSimulationPath(study_path,"input")
            total_cost_parallel_version(x,
                                        new_candidate_grid,
                                        node,
                                        mc_years_optim,
                                        candidates_types,
                                        storage_vol,
                                        list_rewards,
                                        nb_node,
                                        cvar,
                                        opts,
                                        penalty_low,
                                        penalty_high,
                                        penalty_final_level,
                                        storage_annual_cost,
                                        storage_final_level,
                                        step_number)
          })
          parallel::stopCluster(cl)
          
          for (cost in new_grid_costs) {grid_costs <- rbind(grid_costs, cost)}
        } else {
          # without parallel processing
          # loop on the other candidates
          for (candidate_index in 1:length(new_candidate_grid)) {
            # calculate total cost
            if (new_candidate_grid[[candidate_index]] != "not necessary") {
              total_cost_can <- total_cost_loop(area = node,
                                              mc_years = mc_years_optim,
                                              candidate_pool = new_candidate_grid[[candidate_index]],
                                              candidates_types = candidates_types,
                                              storage_vol = storage_vol,
                                              df_reward = list_rewards[[as.character(nb_node)]],
                                              cvar = cvar,
                                              opts = opts,
                                              penalty_low = penalty_low,
                                              penalty_high = penalty_high,
                                              penalty_final_level = penalty_final_level,
                                              storage_annual_cost = storage_annual_cost,
                                              final_level = storage_final_level,
                                              states_step_ratio = (1/step_number))
              print(storage_vol)
              print(new_candidate_grid[[candidate_index]])
              print(total_cost_can)
              grid_costs <- rbind(grid_costs,
                                c(storage_vol, new_candidate_grid[[candidate_index]], total_cost_can))
            }
          }
        }
      }
      
      # find the best candidate at this iteration
      colnames(grid_costs) <- c("Storage", candidates_types$name, "Total_cost")
      best_candidates <- max_candidate(grid_costs)
      
      # update bounds 
      bounds <- new_bounds(best_candidates, candidates_data, new_storage_points)
      candidates_data <- bounds$candidates_data
      storage_bounds <- bounds$storage_bounds
    }
    
    # store output
    output_node[[node]] <- list()
    output_node[[node]]$all_costs <- grid_costs
    output_node[[node]]$best <- max_candidate(grid_costs)
    output_node[[node]]$last_storage_points <- new_storage_points
    output_node[[node]]$last_candidates_data <- candidates_data
    output_node[[node]]$last_rewards <- list_rewards
    
    time2 <- Sys.time()
    output_node[[node]]$optim_time <- time2-time1
    
    print("Best for this node is :")
    print(output_node[[node]]$best)
    
    # store rewards and costs in a file
    to_save <- output_node[[node]]$last_rewards[[as.character(nb_ite)]]$reward
    save(to_save,
         file=paste0(study_path, "/user/Reward_", node, "_", nb_ite, ".RData"))
    
    to_save <- output_node[[node]]$all_costs
    save(to_save, file=paste0(study_path, "/user/All_objectives_", node, "_", unspil_cost, ".RData"))
    
    # add best clusters to node in the study and edit storage size
    if (edit_study) {
    l_old_clusters <- c()
    l_read_clusters <- antaresRead::readClusterDesc(opts=opts)
    l_all_clusters <- as.character(l_read_clusters$cluster)
    
    antaresEditObject::writeIniHydro(area = node, params = c("reservoir capacity" = output_node[[node]][["best"]][["Storage"]]), opts = opts)
    
    for (i in 1:length(l_all_clusters)) {if (l_read_clusters$area[i]==node) {l_old_clusters <- c(l_old_clusters, l_all_clusters[i])}}
    
    for (can in 1:length(candidates_types$index) ){
      power <- output_node[[node]][["best"]][[candidates_types$name[can]]]
      ts_avail <- matrix(power, nrow = 8760, ncol = 1)

      if (paste0(node, "_", candidates_types$name[can]) %in% l_old_clusters) {
        old_power <- subset(antaresRead::readClusterDesc(opts), cluster == paste0(node, "_", candidates_types$name[can]))$nominalcapacity
        antaresEditObject::editCluster(area = node, cluster_name = candidates_types$name[can], nominalcapacity = old_power + power, time_series = ts_avail)
      } else {
        antaresEditObject::createCluster(area = node, cluster_name = candidates_types$name[can],
                                         unitcount = 1L, nominalcapacity = power,
                                         marginal_cost = as.integer(candidates_types$Marg_price[can]),
                                         market_bid_cost = as.integer(candidates_types$Marg_price[can]), must_run = T,
                                         time_series = ts_avail)
        if (candidates_types$type[can] == "cluster flexible") {
          antaresEditObject::editCluster(area = node, cluster_name = candidates_types$name[can], must_run = F)
        }
      }
    }
    }
    }
  return(output_node)
}





#' Compute the list of cluster candidates to study following their bounds
#' 
#' @param candidates_data List of vector of doubles of length 3. One vector of double for each candidate.
#' The vectors of doubles have the form (bound min, bound max, number of points).
#' 
#' @returns a \code{list} of the capacity of each candidate cluster
grid_other_candidates <- function(candidates_data) {
  dim_cand <- length(candidates_data)
  
  # calculate number of points in the grid
  len_grid <- 1
  for (i in 1:dim_cand) {len_grid <- len_grid*candidates_data[[i]][3]}
  new_len <- len_grid
  candidate_grid <- c()
  
  # loop on the candidates
  for (can in 1:dim_cand) {
    new_len <- new_len/candidates_data[[can]][3]
    
    # loop on the points of the grid
    for (i in 1:len_grid) {
      index <- 1 + ((i-1)%/%new_len)%%candidates_data[[can]][3]
      number <- candidates_data[[can]][1] + (candidates_data[[can]][2] - candidates_data[[can]][1])*(index-1)/(candidates_data[[can]][3]-1)
      #if (candidates_data[[can]][3] == 1) {number <- candidates_data[[can]][1]}
      if (can ==1) {candidate_grid[[i]] <- number}
      else {candidate_grid[[i]] <- c(candidate_grid[[i]],number)}
    }
  }
  
  # the "grid" is a list of points containing the dimension of each candidate for this point
  return(candidate_grid)
}



#' Compute reward function with the 5 simulations method.
#' Called for each area of \code{MultiStock_H2_Investment_reward_compute_once}
#' 
#' @param area Character. Name of the area where the reward is computed
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param pumping Boolean. True to take into account the pumping capacity.
#' @param pump_eff Double between 0 and 1. Pumping efficiency ratio. 
#' Get it with \code{antaresWaterValues::getPumpEfficiency()}.
#' @param mcyears Vector of integers. Monte Carlo years to run simulations
#' @param path_to_antares Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param study_path Character. Path to the simulation, argument passed to \code{antaresRead::setSimulationPath}.
#' @param prefix Character. Prefix of the simulation.
#' @param launch_sims Boolean. True to launch simulations, false if simulations already run.
#' 
#' @returns a \code{data_frame} containing the rewards returned by the function \code{antaresWaterValues::get_Reward()}
calculateRewards5Simulations <- function(area,
                                         opts,
                                         pumping = F,
                                         pump_eff = 1,
                                         mcyears,
                                         path_to_antares,
                                         study_path,
                                         prefix,
                                         launch_sims=T) {
  library(dplyr)
  settings_ini <- antaresRead::readIniFile(file.path(opts$studyPath, "settings", "generaldata.ini"))
  settings_ini$`other preferences`$`hydro-pricing-mode` <- "accurate"
  antaresEditObject::writeIni(settings_ini, file.path(opts$studyPath, "settings", "generaldata.ini"),overwrite=T)
  
  constraint_values <- antaresWaterValues::constraint_generator(area=area,
                                                                nb_disc_stock = 11,
                                                                pumping = pumping,
                                                                efficiency = pump_eff,
                                                                opts=opts,mcyears=mcyears) %>%
    dplyr::filter(sim %in% c("u_4","u_5","u_6","u_7","u_8"))
  
  # constraint_values <- antaresWaterValues::constraint_generator(area=area,
  #                                           nb_disc_stock = 11,
  #                                           pumping = pumping,
  #                                           pumping_efficiency = pump_eff,
  #                                           opts=opts,mcyears=mcyears) %>%
  #   dplyr::filter(sim %in% c("u_6"))
  
  
  ### SIMULATIONS
  
  {start.time <- Sys.time()
    
    simulation_res <- antaresWaterValues::runWaterValuesSimulation(
      constraint_values = constraint_values,
      area = area,
      mcyears = mcyears,
      path_solver = path_to_antares,
      overwrite = T,
      opts = opts,
      file_name = paste0(area,prefix),
      pumping = pumping,
      efficiency = pump_eff,
      show_output_on_console = FALSE,
      launch_simulations = c(rep(launch_sims, 5)),
      expansion = T
    )
    
    end.time <- Sys.time()
    simulation_res$simulation_time <- end.time - start.time
    save(simulation_res,file=paste0(study_path,"/user/",area,"_",prefix,".RData"))}
  
  {
    output_dir <- list.dirs(paste0(study_path,"/output"),recursive = F)
    for (s in simulation_res$simulation_names){
      output_info = antaresRead::readIniFile(paste0(output_dir[stringr::str_detect(output_dir,paste0(s,"$"))]
                                                    ,"/info.antares-output"))
      output_info$general$mode <- "Economy"
      antaresEditObject::writeIniFile(output_info,paste0(output_dir[stringr::str_detect(output_dir,paste0(s,"$"))]
                                                         ,"/info.antares-output"),
                                      overwrite = T)
    }
  }
  
  computing_time <- data.frame()
  df_vu <- data.frame()
  df_reward <- data.frame()
  
  
  {start.time <- Sys.time()
    opts <- antaresRead::setSimulationPath(study_path, "input")
    load(paste0(study_path,"/user/",area,"_",prefix,".RData"))
    
    max_hydro <- antaresWaterValues::get_max_hydro(area,opts,timeStep = "hourly")
    controls_reward_calculation <- antaresWaterValues::constraint_generator(area=area,
                                                                            nb_disc_stock = 51,
                                                                            pumping = pumping,
                                                                            efficiency = pump_eff,
                                                                            opts=opts,mcyears=mcyears)
    if (("mcYear" %in% names(simulation_res$simulation_values))&!("mcYear" %in% names(controls_reward_calculation))){
      controls_reward_calculation <- dplyr::cross_join(controls_reward_calculation,
                                                       data.frame(mcYear=mcyears))
    }
    
    # controls_reward_calculation <- rbind(simulation_res$simulation_values,controls_reward_calculation) %>%
    #   dplyr::select(-c("sim")) %>%
    #   dplyr::distinct() %>%
    #   dplyr::arrange(.data$week,.data$u)
    
    reward_db <- antaresWaterValues::get_Reward(simulation_values = simulation_res$simulation_values,
                                                simulation_names = simulation_res$simulation_names,
                                                opts=opts,
                                                area = area, mcyears = mcyears,
                                                method_old = F, max_hydro = max_hydro,
                                                possible_controls =controls_reward_calculation,
                                                expansion = simulation_res$expansion,
                                                efficiency = pump_eff)#$reward
    reward <- reward_db$reward
    reward <- reward  %>% dplyr::group_by(.data$timeId,.data$mcYear) %>%
      dplyr::mutate(marg=(.data$reward-dplyr::lag(.data$reward))/(.data$control-dplyr::lag(.data$control)))
    end.time <- Sys.time()
    reward_time <- end.time - start.time
    print(reward_time)}
  
  return(reward_db)
}




#' Computes the total reward for a specific candidate following the reward function
#' 
#' @param area Character. Name of the area where the total reward is computed
#' @param mc_years Vector of integers. Monte Carlo years to compute reward
#' @param candidate_pool Vector of integers. Capacity of the cluster candidates
#' @param candidates_types Data_frame with column names : c("index", "name", "type", "TOTEX", "Marg_price").
#' It is a parameter of \code{MultiStock_H2_Investment_reward_compute_once}.
#' @param storage_vol Integer. Volume of the storage candidate
#' @param df_reward List. Rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param penalty_low Integer. Penalty for lower guide curve.
#' @param penalty_high Integer. Penalty for higher guide curve.
#' @param penalty_final_level Integer. Penalty for higher and lower final level.
#' @param storage_annual_cost Numeric. Total annual cost of the storage candidate in eur/MWh
#' @param final_level Numeric in [0, 100]. Final and initial level in % of H2 storage.
#' @param states_step_ratio Numeric. Step number for reward functions. Must be in adequacy with step ratio from \code{df_reward}.
#' 
#' @returns The total reward for the specified candidates in euros. Because of the Bellman values it is usually negative and large
total_cost_loop <- function(area,
                            mc_years,
                            candidate_pool,
                            candidates_types,
                            storage_vol,
                            df_reward,
                            cvar,
                            opts,
                            penalty_low,
                            penalty_high,
                            penalty_final_level,
                            storage_annual_cost,
                            final_level,
                            states_step_ratio) {

  for (can in 1:length(candidate_pool)) {candidate_pool[can] <- as.integer(candidate_pool[can])}

  new_rewards <- df_reward
  if (length(df_reward$reward$control) == 0) {
    print("No flexibility or additional energy required for the system")}

  # change reward function to include other candidates
  if (!is.null(candidates_types)) {
    for (can in 1:length(candidates_types$index)) {
      index <- candidates_types$index[can]
      type <- candidates_types$type[can]
      
      # update reward function following the type of the candidate
      if (type == "cluster bande" & length(new_rewards$reward$timeId) > 0 & candidate_pool[can] > 0) {
        new_rewards <- update_reward_cluster_bande(candidate_pool[can], new_rewards, mc_years)
        print(paste0(candidates_types$name[can], " updated"))
      }
      if (type == "cluster flexible"  & length(new_rewards$reward$timeId) > 0 & candidate_pool[can] > 0) {
        new_rewards <- update_reward_cluster_flexible(candidate_pool[can], as.numeric(candidates_types$Marg_price[can]), new_rewards, mc_years)
        print(paste0(candidates_types$name[can], " updated"))
      }
    }
  }
  
  if (length(new_rewards$reward$control) == 0) {
    print("The must-run clusters are sufficient for the system")
    total_cost <- - storage_annual_cost*storage_vol 
    for (can in 1:length(candidates_types$index)) {
      total_cost <- total_cost - as.numeric(candidates_types$TOTEX[can])*candidate_pool[can]
    }
    return(total_cost)
  }
  
  force_final_level <- T
  if (penalty_final_level == 0) {force_final_level <- F}
  
  if (all(new_rewards$reward$control == 0)) {print("cluster bande trop grand")}

  # call grid_matrix to compute lb
  res <- antaresWaterValues::Grid_Matrix(area = area,
                        reward_db = new_rewards,
                        mcyears = mc_years,
                        states_step_ratio = states_step_ratio,
                        reservoir_capacity = storage_vol,
                        cvar_value = cvar,
                        opts = opts,
                        penalty_low = penalty_low,
                        penalty_high = penalty_high,
                        force_final_level = force_final_level,
                        final_level = final_level,
                        penalty_final_level_high = penalty_final_level,
                        penalty_final_level_low = penalty_final_level)

  op_cost <- res$lower_bound
  
  # compute total cost
  total_cost <- op_cost - storage_annual_cost*storage_vol
  for (can in 1:length(candidates_types$index)) {
    total_cost <- total_cost - as.numeric(candidates_types$TOTEX[can])*candidate_pool[can]
    if (candidates_types$type[can] == "cluster bande") {
      total_cost <- total_cost - as.numeric(candidates_types$Marg_price[can])*candidate_pool[can]*8736
    }
  }
  
  return(total_cost)
}



#' Modify the reward function to take into account the effects of a must-run cluster.
#' 
#' @param power Integer. Capacity of the cluster.
#' @param reward_init List. Rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#' @param mc_years Vector of integers. Monte Carlo years of the rewards.
#' 
#' @returns a \code{list} of rewards and decision space, like \code{reward_init}.
update_reward_cluster_bande <- function(power, reward_init, mc_years) {
  new_reward <- reward_init$reward
  new_decision <- reward_init$decision_space
  volume_change <- 168*power
  to_delete <- c()
  
  # update control points
  n <- length(new_decision$u)
  for (point in 1:n) {
    new_u <- new_decision$u[point] - volume_change
    if (new_u >= 0) {
      for (y in 1:length(mc_years)) {new_reward$control[n*(y-1)+point] <- new_u}
      new_decision$u[point] <- new_u
    }
    else {
      for (y in 1:length(mc_years)) {
        to_delete <- c(to_delete, n*(y-1)+point)
      }
    }
  }
  
  # delete non positive control points
  if (length(to_delete)>0) {
    new_reward <- new_reward[-to_delete,]
    new_decision <- new_decision[-to_delete,]
  }
  
  reward_fin <- list()
  reward_fin$decision_space <- new_decision
  reward_fin$reward <- new_reward
  return(reward_fin)
}



#' Modify the reward function to take into account the effects of a non must-run cluster, with a speciifc capcaity and marginal cost.
#' 
#' @param power Integer. Capacity of the cluster.
#' @param marg_cost Numeric. Marginal cost (and market bid) of the cluster in eur/MWh.
#' @param reward_init List. Rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#' @param mc_years Vector of integers. Monte Carlo years of the rewards.
#' 
#' @returns a \code{list} of rewards and decision space, like \code{reward_init}.
update_reward_cluster_flexible <- function(power, marg_cost, reward_init, mcYear) {
  old_reward <- reward_init$reward
  new_reward <- data.frame()
  
  for (year in mcYear) {
    for (week in 1:max(old_reward$timeId)) {
      #separate each week from another
      reward_week <- subset(subset(old_reward, timeId == week), mcYear == year)
      
      # compute differential reward
      diff_reward_week <- c()
      for (point in 2:length(reward_week$timeId)) {
        diff_reward_week <- c(diff_reward_week,
                              (reward_week$reward[point] - reward_week$reward[point-1])/(reward_week$control[point]-reward_week$control[point-1]))
      }

      # Change reward following the case
      if (max(diff_reward_week) > marg_cost) {
        if (min(diff_reward_week) > marg_cost) {
          # the cluster is used at maximum power
          power_max <- power*168
        }
        else {
          # the cluster is used until it is too expensive or at maximum power
          for (i in 1:length(diff_reward_week)) {
            if (diff_reward_week[i] > marg_cost) {
              power_max <- reward_week$control[i]
            }
            power_max <- min(power_max, power*168)
          }
        }
        
        # modification of the reward derivative until we reach power_max
        pi <- length(reward_week$timeId) - 1
        flag <- T
        power_attrib <- 0
        old_diff_reward_week <- diff_reward_week
        pmax <- 0
        for (pj in rev(1:(length(reward_week$timeId)-1))) {
          if (flag & old_diff_reward_week[pi] > marg_cost) {
            delta <- reward_week$control[pj+1] - reward_week$control[pj]
            power_attrib <- power_attrib + delta
            if (power_attrib > power_max) {
              flag <- F
              diff_reward_week[pj] <- (marg_cost*(delta - power_attrib + power_max) + old_diff_reward_week[pi]*(power_attrib - power_max))/delta
              pi <- pi - 1
              pmax <- pj
              poids_last <- delta - power_attrib + power_max
            } else {
              diff_reward_week[pj] <- marg_cost
            }
          }else {
            if (pj<pmax & pi>1) {
              diff_reward_week[pj] <- (old_diff_reward_week[pi+1]*poids_last + old_diff_reward_week[pi]*(delta-poids_last))/delta
                pi <- pi - 1
            } else {
              diff_reward_week[pj] <- old_diff_reward_week[pi]
              pi <- pi - 1
            }
          }
        }

        "
        # modification of the reward derivative until we reach power_max
        for (point in 2:length(reward_week$timeId)) {
          if (reward_week$control[point] < power_max) {
            diff_reward_week[point] <- marg_cost
          }
        }
        
        # sort reward derivative
        diff_reward_week <- sort(diff_reward_week, decreasing = T)
        "
        # modification of the reward following its new derivative
        for (point in rev(1:(length(reward_week$timeId)-1))) {
          if (point > length(diff_reward_week)) {
            reward_week$reward[point] <- reward_week$reward[point+1]
          } else {
            reward_week$reward[point] <- reward_week$reward[point+1] - diff_reward_week[point]*(reward_week$control[point+1]-reward_week$control[point])
          }
        }
      }
      
      # update week
      if (length(new_reward) >0) {new_reward <- rbind(new_reward, reward_week)}
      else {new_reward <- reward_week}
    }
  }
  colnames(new_reward) <- colnames(old_reward)
  reward_init$reward <- new_reward
  
  return(reward_init)
}



#' Compute the candidate with maximal reward
#' 
#' @param grid_costs List. The firsts columns correspond to the candidates storage and cluster.
#' The last column contains the total reward under names "Total_cost".
#' 
#' @returns A \code{list} containing only the row of the best candidate from \code{grid_costs}.
max_candidate <- function(grid_costs) {
  best_index <- 1
  best <- grid_costs$Total_cost[1]
  for (i in 1:length(grid_costs$Total_cost)) {
    if (grid_costs$Total_cost[i] > best) {
      best_index <- i
      best <- grid_costs$Total_cost[i]
    }
  }
  return(subset(grid_costs[best_index,], select = -Total_cost))
}


#' Updates the candidates bounds at the end of an iteration.
#' 
#' @param best_candidate
#' @param candidates_data List of vector of doubles of length 3. One vector of double for each cluster candidate.
#' The vectors of doubles have the form (bound min, bound max, number of points).
#' The number of points must be >3 to update bounds at each iterations and approach solution.
#' @param storage_points Vector of integers. Storage volumes candidates at these iteration
#' 
#' @returns A \code{list} with :
#' \item{storage_bounds}{Vector of integers. Storage volumes candidates at next iteration.}
#' \item{candidates_data}{List. It is of the same form as the \code{candidates_data} argument with new clusters candidates.}
new_bounds <- function(best_candidate, candidates_data, storage_points) {
  output <- list()

  # update storage bounds
  stor_diff_init <- storage_points[2] - storage_points[1]
  if (length(storage_points) > 1) {
    new_storage_bounds <- c(max(best_candidate$Storage - stor_diff_init, storage_points[1]),
                            min(best_candidate$Storage + stor_diff_init, storage_points[length(storage_points)]))
  } else {new_storage_bounds <- c(storage_points, storage_points)}
  output$storage_bounds <- new_storage_bounds
  
  # update other candidates bounds 
  new_can_data <- list()
  for (can in 1:length(candidates_data)) {
    can_diff_init <- (candidates_data[[can]][2]-candidates_data[[can]][1])/(candidates_data[[can]][3]-1)
    new_can_data[[can]] <- c(max(best_candidate[1, 1+can] - can_diff_init, candidates_data[[can]][1]),
                             min(best_candidate[1, 1+can] + can_diff_init, candidates_data[[can]][2]),
                             candidates_data[[can]][3])
  }
  output$candidates_data <- new_can_data
  
  return(output)
}



#' Computes the total reward for a list of candidate following the reward function, with parallel processing.
#' The storage volume is constant.
#' These function is called inside \code{parLapply()}.
#'   
#' @param candidate_index Integer. Index of the candidate to study in \code{new_candidate_grid}. Argument passed to \code{parLapply()}
#' @param new_candidate_grid List of cluster candidates to study at these iteration.
#' @param node Character. Name of the area where the total reward is computed
#' @param mc_years_optim Vector of integers. Monte Carlo years to compute reward
#' @param candidates_types Data_frame with column names : c("index", "name", "type", "TOTEX", "Marg_price").
#' It is a parameter of \code{MultiStock_H2_Investment_reward_compute_once}.
#' @param storage_vol Integer. Volume of the storage candidate
#' @param list_rewards List. List of rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#' Rewards and decision spaces are given for each area marked by their number.
#' @param nb_node Integer. Number of the specific area to get corresponding reward from \code{list_rewards}
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param penalty_low Integer. Penalty for lower guide curve.
#' @param penalty_high Integer. Penalty for higher guide curve.
#' @param penalty_final_level Integer. Penalty for higher and lower final level.
#' @param storage_annual_cost Numeric. Total annual cost of the storage candidate in eur/MWh
#' @param storage_final_level Numeric in [0, 100]. Final and initial level in % of H2 storage.
#' @param step_number Integer. Step number for reward functions. 
#' Must be in adequacy with the step number if the reward function.
#' 
#' @returns The total reward for the specified candidates in euros. Because of the Bellman values it is usually negative and large
total_cost_parallel_version <- function(candidate_index,
                                        new_candidate_grid,
                                        node,
                                        mc_years_optim,
                                        candidates_types,
                                        storage_vol,
                                        list_rewards,
                                        nb_node,
                                        cvar,
                                        opts,
                                        penalty_low,
                                        penalty_high,
                                        penalty_final_level,
                                        storage_annual_cost,
                                        storage_final_level,
                                        step_number) {
  
  # calculate total cost
  new_grid_costs <- data.frame(matrix(ncol = 2+length(candidates_types$index), nrow=0))
  if (new_candidate_grid[[candidate_index]] != "not necessary") {
    total_cost_can <- total_cost_loop(area = node,
                                      mc_years = mc_years_optim,
                                      candidate_pool = new_candidate_grid[[candidate_index]],
                                      candidates_types = candidates_types,
                                      storage_vol = storage_vol,
                                      df_reward = list_rewards[[as.character(nb_node)]],
                                      cvar = cvar,
                                      opts = opts,
                                      penalty_low = penalty_low,
                                      penalty_high = penalty_high,
                                      penalty_final_level = penalty_final_level,
                                      storage_annual_cost = storage_annual_cost,
                                      final_level = storage_final_level,
                                      states_step_ratio = (1/step_number))
    print(storage_vol)
    print(new_candidate_grid[[candidate_index]])
    print(total_cost_can)
    new_grid_costs <- rbind(new_grid_costs,
                        c(storage_vol, new_candidate_grid[[candidate_index]], total_cost_can))
  }
  colnames(new_grid_costs) <- c("Storage", candidates_types$name, "Total_cost")
  return(new_grid_costs)
}








