#' Compute optimal candidates for H2 system
#'
#' Search optimal solution for bounded candidates for the H2 systems including storage, must-run clusters and flexibles clusters.
#'
#' @param areas_invest Vector of characters of the names of areas to optimize.
#' @param max_ite Integer. Maximum number of iterations for each area.
#' @param storage_bounds Vector of integers of length 2, with the form (min, max).
#' @param storage_points_nb Integer. Number of storage points to test at each iteration.
#' Must be >3 to update bounds at each iterations and approach solution.
#' @param candidates_types_gen Data_frame with column names : \code{c(index, name, type, TOTEX, Marg_price, Part_fixe, Prix_fixe,
#' Borne_min, Borne_max, Points_nb, Zone)}. Each row describes a cluster candidate. The index should correspond to the index of the candidate in \code{candidates_data}.
#' The name is a character, the type is either \code{"cluster_flexible"} or \code{"cluster_bande"}, TOTEX is in eur/MW/year, Marg_price is in eur/MWh.
#' Part_fixe is the fixed part for a variable cluster between 0 and 1, Prix_fixe in eur/MWh is its price.
#' Borne_min and Borne_max are in MW, Points_nb is an integer (number of candidates tested at each iteration, it should be at least 4).
#' Zone is a character containing the name of the area where to propose the candidate.
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
#' @param parallelprocess Boolean. True to compute Water values with parallel processing.
#' @param nb_sockets Integer. Number of sockets for parallel processing
#' @param unspil_cost Numeric. Unspilled energy cost in eur/MW for all concerned areas.
#' @param edit_study Boolean. True to edit study with optimal candidates.
#' @param back_to_first_node Boolean. True to play again first node at the end. There is no possibility to go uninvest.
#'
#' @returns a \code{list} containing for each area detailed results (best candidate, all total costs, reward function, optimization time)
#'
#' @export
MultiStock_H2_Investment_reward_compute_once <- function(areas_invest,
                                                         max_ite,
                                                         storage_bounds,
                                                         storage_points_nb,
                                                         candidates_types_gen,
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
                                                         nb_sims = 51,
                                                         parallelprocess = F,
                                                         nb_sockets = 0,
                                                         unspil_cost = 3000,
                                                         edit_study = F,
                                                         back_to_first_node = F) {

  # initialization

  output_node <- list()
  storage_bounds_init <- storage_bounds
  nb_node <- 0
  df_previous_cuts <- NULL

  list_pumping <- c()
  list_efficiency <- c()
  browser()
  for (i in 1:length(areas_invest)){
    pumping = max(get_max_hydro(area=areas_invest[i],opts=opts,timeStep = "weekly")$pump)>0
    list_pumping <- c(list_pumping,pumping)
    list_efficiency <- c(list_efficiency,getPumpEfficiency(area=areas_invest[i],opts=opts))
  }
  names(list_efficiency) <- areas_invest
  names(list_pumping) <- areas_invest

  # add fixed part of flexible cluster if necessary
  n <- length(candidates_types_gen$index)
  for (cl in 1:n) {
    if (candidates_types_gen$Part_fixe[cl] > 0 & candidates_types_gen$type[cl] == "cluster flexible") {
      new_cluster <- c(length(candidates_types_gen$index)+1, paste0(candidates_types_gen$name[cl], "_fixe"), "cluster bande",
                       candidates_types_gen$TOTEX[cl], candidates_types_gen$Prix_fixe[cl], 0, 0,0,0,0,candidates_types_gen$Zone[cl])
      candidates_types_gen <- rbind(candidates_types_gen, new_cluster)
      }
  }

  # edit unsupplied energy cost
  areas_unsp <- names(antaresRead::readIni("input/thermal/areas", opts)$unserverdenergycost)
  df_econ_options <- data.frame(areas_unsp, c(seq(unspil_cost, unspil_cost, length.out = length(areas_unsp))))
  colnames(df_econ_options) <- c("area", "average_unsupplied_energy_cost")
  antaresEditObject::writeEconomicOptions(df_econ_options, opts)

  # main loop on areas
  areas_loop = areas_invest
  if (back_to_first_node){
    areas_loop = c(areas_loop,areas_invest[[1]])
  }
  for (node in areas_loop) {
    time1 <- Sys.time()
    nb_ite <- 0
    nb_node <- nb_node +1

    candidates_types <- subset(candidates_types_gen, .data$Zone == node)

    grid_costs <- data.frame(matrix(ncol = 2+length(candidates_types$index), nrow=0))

    storage_bounds <- storage_bounds_init

    # getting reward functions
    list_rewards <- list()

    candidates_data <- c()
    for (can in 1:length(candidates_types$index)) {
      if (!grepl("_fixe", candidates_types$name[can])) {
      candidates_data[[can]] <- c(as.numeric(candidates_types$Borne_min[can]), as.numeric(candidates_types$Borne_max[can]),
                                  as.numeric(candidates_types$Points_nb[can]))
      }
    }

    if (length(areas_invest) > 1) {
      simulation_point_res <-
        calculateRewards5Simulations_MultiStock(area =node,
                                                list_areas = areas_invest,
                                                list_pumping = list_pumping,
                                                list_efficiency = list_efficiency,
                                                opts = opts,
                                                mcyears = mc_years_optim,
                                                path_to_antares = path_to_antares,
                                                study_path = study_path,
                                                prefix=paste0("unsp", as.character(unspil_cost), "_", nb_node),
                                                launch_sims=launch_sims,
                                                penalty_low=penalty_low,
                                                penalty_high=penalty_high,
                                                penalty_final_level=penalty_final_level,
                                                cvar=cvar,
                                                sim_number = nb_sims,
                                                df_previous_cuts = df_previous_cuts)
    } else {
      simulation_point_res <-
        calculateRewardsSimulations(area=node,
                                     opts=opts,
                                     pumping = list_pumping[[node]],
                                     pump_eff = list_efficiency[[node]],
                                     mcyears = mc_years_optim,
                                     path_to_antares =  path_to_antares,
                                     study_path = study_path,
                                     prefix=paste0("unsp", as.character(unspil_cost), "_", nb_node),
                                     launch_sims=launch_sims,
                                     sim_number = nb_sims)
    }

    if (is.null(df_previous_cuts)) {
      df_previous_cuts <- simulation_point_res$reward
      df_previous_cuts <- dplyr::mutate(df_previous_cuts, area=node)
      df_previous_cuts <- df_previous_cuts %>% dplyr::rename("week"="timeId", "u"="control")
      df_previous_cuts <- df_previous_cuts %>%
        dplyr::group_by(.data$week,.data$mcYear) %>%
        dplyr::mutate(marg=(.data$reward-dplyr::lag(.data$reward))/(.data$u-dplyr::lag(.data$u)))
    } else {
      df_current_cuts <- simulation_point_res$reward
      df_current_cuts <- dplyr::mutate(df_current_cuts, area=node)
      df_current_cuts <- df_current_cuts %>%
        dplyr::rename("week"="timeId", "u"="control") %>%
        dplyr::group_by(.data$week,.data$mcYear) %>%
        dplyr::mutate(marg=(.data$reward-dplyr::lag(.data$reward))/(.data$u-dplyr::lag(.data$u)))
      df_previous_cuts <- rbind(df_previous_cuts, df_current_cuts)
    }

    # store results edited by area index
    list_rewards[[as.character(nb_node)]] <- simulation_point_res

    # loop on the candidates grids
    while (nb_ite < max_ite) {
      nb_ite <- nb_ite + 1

      # compute storage volumes to evaluate
      new_storage_points <- c(seq(storage_bounds[1], storage_bounds[2], length.out = storage_points_nb))
      for (sto in 1:length(new_storage_points)) {new_storage_points[sto] <- as.integer(new_storage_points[sto])}

      # compute grid of the other candidates
      second_candidate_grid <- grid_other_candidates(candidates_data)
      # add fixed part of flexible cluster to candidates
      for (i in 1:length(second_candidate_grid)) {
        for (j in 1:length(candidates_types$index)) {
          if (candidates_types$Part_fixe[j] > 0) {
            second_candidate_grid[[i]] <- c(second_candidate_grid[[i]], second_candidate_grid[[i]][as.integer(candidates_types$index[j])]*as.numeric(candidates_types$Part_fixe[j]))}
        }
      }

      new_candidate_grid <- second_candidate_grid

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
                                        final_level)
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
                                              final_level = final_level)
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
    to_save <- output_node[[node]]$last_rewards[[as.character(nb_node)]]$reward
    save(to_save,
         file=paste0(study_path, "/user/Reward_", node, "_", nb_node, ".RData"))

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
        old_power <- subset(antaresRead::readClusterDesc(opts), .data$cluster == paste0(node, "_", candidates_types$name[can]))$nominalcapacity
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
#' @param sim_number Integer. Number of simulations.
#'
#' @returns a \code{data_frame} containing the rewards returned by the function \code{antaresWaterValues::get_Reward()}
calculateRewardsSimulations <- function(area,
                                         opts,
                                         pumping = F,
                                         pump_eff = 1,
                                         mcyears,
                                         path_to_antares,
                                         study_path,
                                         prefix,
                                         launch_sims=T,
                                         sim_number) {

  constraint_values <- antaresWaterValues::constraint_generator(area=area,
                                                                nb_disc_stock = sim_number,
                                                                pumping = pumping,
                                                                efficiency = pump_eff,
                                                                opts=opts,mcyears=mcyears)


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
      launch_simulations = c(rep(launch_sims, sim_number)),
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







#' Compute reward function with the 5 simulations method.
#' Called for each area of \code{MultiStock_H2_Investment_reward_compute_once}.
#' If they are several areas, the trajectories of the other storage are fixed on their optimal trend.
#'
#' @param area Character. Name of the area where the reward is computed
#' @param list_areas Vector of characters of the names of areas to optimize.
#' @param list_pumping Vector of boolean. True if pumping for the area with corresponding index in \code{list_areas}.
#' @param list_efficiency Vector of numeric with pumping efficiency for the area with corresponding index in \code{list_areas}.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param mcyears Vector of integers. Monte Carlo years to run simulations
#' @param path_to_antares Character containing the Antares Solver path, argument passed to \code{\link[antaresEditObject]{runSimulation}}.
#' @param study_path Character. Path to the simulation, argument passed to \code{antaresRead::setSimulationPath}.
#' @param prefix Character. Prefix of the simulation.
#' @param launch_sims Boolean. True to launch simulations, false if simulations already run.
#' @param penalty_low Integer. Penalty for lower guide curve.
#' @param penalty_high Integer. Penalty for higher guide curve.
#' @param penalty_final_level Integer. Penalty for higher and lower final level.
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param sim_number Integer. Number of simulations.
#' @param df_previous_cuts Data frame containing previous estimations of cuts
#'
#' @returns a \code{data_frame} containing the rewards returned by the function \code{antaresWaterValues::get_Reward()}
calculateRewards5Simulations_MultiStock <- function(area,
                                                        list_areas,
                                                        list_pumping,
                                                        list_efficiency,
                                                        opts,
                                                        mcyears,
                                                        path_to_antares,
                                                        study_path,
                                                        prefix,
                                                        launch_sims=T,
                                                        penalty_low,
                                                        penalty_high,
                                                        penalty_final_level,
                                                        cvar,
                                                        sim_number = 5,
                                                        df_previous_cuts = NULL)  {

  # compute list of inflows
  list_inflow = list()
  list_capacity = list()
  list_backup = list()
  df_rewards <- data.frame()
  for (j in seq_along(list_areas)){
    node = list_areas[[j]]
    list_inflow[[j]] <- antaresWaterValues::get_inflow(area=node, opts=opts,mcyears=mcyears)
    list_capacity[[j]] <- antaresWaterValues::get_reservoir_capacity(area = node, opts = opts)
    list_backup[[j]] = getBackupData(area=node,mcyears,opts)
  }
  names(list_inflow) = list_areas
  names(list_capacity) = list_areas

  constraint_values <- data.frame()
  for (j in 1:length(list_areas)){
    initial_traj = data.frame()
    a <- list_areas[j]
    if (a != area) {

      if (!is.null(df_previous_cuts)) {
        df_previous_cut <- dplyr::filter(df_previous_cuts, area == a)
        if (length(df_previous_cut$mcYear) == 0) {df_previous_cut <- NULL}
      } else {df_previous_cut <- NULL}


      max_hydro <- antaresWaterValues::get_max_hydro(a, opts)
      max_hydro_weekly <- max_hydro %>%
        dplyr::mutate(timeId=(.data$timeId-1)%/%168+1) %>%
        dplyr::group_by(.data$timeId) %>%
        dplyr::summarise(pump=sum(.data$pump),turb=sum(.data$turb),.groups = "drop")

      if (!is.null(df_previous_cut)) {

        # initialize controls
        max_hydro <- dplyr::rename(max_hydro,"P_max"="pump","T_max"="turb")
        controls <- antaresWaterValues::constraint_generator(area = area,nb_disc_stock = 51,
                                                             pumping = list_pumping[[j]],opts = opts,
                                                             efficiency = list_efficiency[[j]],
                                                             max_hydro_weekly = max_hydro_weekly,inflow = list_inflow[[area]],
                                                             reservoir_capacity = list_capacity[[area]])
        controls <- tidyr::drop_na(controls) %>%
          dplyr::cross_join(data.frame(mcYear=mcyears))

        #initialize trajectory
        max_hydro <- get_max_hydro(list_areas[[j]],opts,timeStep = "weekly")

        initial_traj <- list_inflow[[j]] %>%
          dplyr::filter(.data$tsId %in% mcyears, .data$timeId<=52) %>%
          dplyr::left_join(max_hydro,by = dplyr::join_by("timeId")) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(hydroStorage = .data$hydroStorage) %>%
          dplyr::select(c("timeId","tsId","hydroStorage")) %>%
          dplyr::rename("u"="hydroStorage","week"="timeId","mcYear"="tsId") %>%
          dplyr::mutate(area=list_areas[[j]]) %>%
          dplyr::ungroup() %>%
          rbind(initial_traj)

        # if we have previous cuts for other nodes

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
        df_rewards <- df_rewards %>% tidyr::drop_na(reward)


        reward <- df_rewards %>%
          dplyr::filter(.data$area == a) %>%
          dplyr::group_by(.data$mcYear,.data$week,.data$u) %>%
          dplyr::summarise(reward=min(reward),.groups="drop")
        reward <- reward %>% dplyr::group_by(.data$week)

        final_level <- antaresWaterValues::get_initial_level(a,opts)
        level_init <- final_level*list_capacity[[a]]/100

        results <- updateWatervalues(reward=reward,controls=controls,area=a,
                                          mcyears=mcyears,
                                          opts=opts,states_step_ratio=(1/51),
                                          pump_eff=pump_eff,
                                          penalty_low=penalty_low,
                                          penalty_high=penalty_high,
                                          inflow=list_inflow[[a]],
                                          max_hydro_weekly = max_hydro_weekly,
                                          niveau_max=list_capacity[[a]],
                                          cvar_value = cvar,
                                          force_final_level = T,
                                          final_level = final_level,
                                          penalty_final_level = penalty_final_level)

        # Compute optimal trend levels

        levels <- getOptimalTrend_var(level_init = level_init,
                                      watervalues=results$watervalues,
                                      mcyears = mcyears,
                                      controls = controls,
                                      niveau_max = list_capacity[[a]],
                                      penalty_low = penalty_low,
                                      penalty_high = penalty_high,
                                      penalty_final_level = penalty_final_level,
                                      final_level = final_level,
                                      max_hydro_weekly = max_hydro_weekly,
                                      pump_eff = pump_eff)

      } else {
        # If there is no previous cuts
        levels = max_hydro_weekly %>%
          dplyr::mutate(constraint = -.data$pump*pump_eff,
                        true_constraint = .data$constraint,
                        lev = NA) %>%
          dplyr::select(-c("turb","pump")) %>%
          dplyr::rename(week=.data$timeId) %>%
          dplyr::cross_join(data.frame(scenario = seq_along(mcyears),mcYear=mcyears))
      }

      levels <- levels %>%
        dplyr::select(c("week", "constraint","mcYear")) %>%
        dplyr::filter(.data$week>0) %>%
        dplyr::rename("u"="constraint")

      levels <- levels %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(u = mean(.data$u)) %>%
        dplyr::mutate(area = a)

      if (!is.null(df_previous_cut)) {
        levels <- rbind(levels, dplyr::filter(initial_traj,.data$area!=a))
      }

      levels_with_sim <- data.frame()
      for (simname in 1:sim_number) {
        levels_with_val <- levels %>%
          dplyr::mutate(sim = stringr::str_c("u_",simname))
        levels_with_sim <- dplyr::bind_rows(levels_with_sim, levels_with_val)
      }
      levels <- levels_with_sim

      constraint_values <- rbind(constraint_values, levels)
    }
  }

  cplus <- antaresWaterValues::constraint_generator(area=area,
                                                    nb_disc_stock = sim_number,
                                                    pumping = list_pumping[[area]],
                                                    efficiency = list_efficiency[[area]],
                                                    opts=opts,mcyears=mcyears) %>%
    dplyr::mutate(area = area)

  constraint_values <- rbind(constraint_values,cplus)


  for (j in seq_along(list_areas)){
    a <- list_areas[[j]]
    backup <- list_backup[[j]]

    opts <- setupWaterValuesSimulation(
      area = a,
      overwrite = TRUE,
      opts = opts,
      pumping=list_pumping[[j]],
      efficiency=list_efficiency[[j]],
      backup = backup
    )

  }
  opts <- setWaterValuesDistrict(opts)


  ### SIMULATIONS
  if (launch_sims==T) {
    simulation_names <- vector(mode = "character", length = sim_number)

    for (ite in 1:sim_number) {
      name_sim <- paste0(prefix, "_",area, "_wv_sim_u", as.character(ite))
      for (j in seq_along(list_areas)){
        constraint_value <- dplyr::filter(constraint_values,
                                          .data$area==list_areas[[j]]) %>%
          dplyr::select(-c("area")) %>%
          dplyr::filter(.data$sim == stringr::str_c("u_",ite))
        generate_rhs_bc(constraint_value=constraint_value,area=list_areas[[j]],
                        opts=opts)
      }
      launchSimulation(opts,ite,name_sim,path_to_antares,TRUE,FALSE,constraint_value)

      simulation_names[ite] <- name_sim

      clear_scenario_builder(opts)
    }

    for (j in seq_along(list_areas)){
      a = list_areas[[j]]
      resetStudy(opts,a,list_pumping[[j]],list_backup[[j]])
    }

    clear_scenario_builder(opts)

    simulation_res <- list(
      simulation_names = simulation_names,
      simulation_values = constraint_values,
      area = area,
      mcyears = mcyears,
      pumping = list_pumping[[area]],
      efficiency = list_efficiency[[area]],
      expansion = T
    )
    save(simulation_res,file=paste0(study_path,"/user/",prefix, "_", area, "_",".RData"))
  } else {

    for (j in seq_along(list_areas)){
      a = list_areas[[j]]
      resetStudy(opts,a,list_pumping[[j]],list_backup[[j]])
    }

    opts <- antaresRead::setSimulationPath(study_path, "input")

    simulation_res <- antaresWaterValues::runWaterValuesSimulationMultiStock(
      list_areas = list_areas,
      list_pumping = list_pumping,
      list_efficiency = list_efficiency,
      constraint_values = constraint_values,
      mcyears = mcyears,
      path_solver = path_to_antares,
      overwrite = T,
      opts = opts,
      file_name = paste0(prefix, "_", area),
      show_output_on_console = FALSE,
      launch_simulations = c(rep(F, sim_number)),
      expansion = T)

    simulation_names <- simulation_res$simulation_names
  }

  {
    output_dir <- list.dirs(paste0(study_path,"/output"),recursive = F)
    for (s in simulation_names){
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
    load(paste0(study_path,"/user/", prefix, "_", area, "_",".RData"))

    max_hydro <- antaresWaterValues::get_max_hydro(area,opts,timeStep = "hourly")
    controls_reward_calculation <- antaresWaterValues::constraint_generator(area=area,
                                                                            nb_disc_stock = 51,
                                                                            pumping = list_pumping[[area]],
                                                                            efficiency = list_efficiency[[area]],
                                                                            opts=opts,mcyears=mcyears)
    if (("mcYear" %in% names(constraint_values))&!("mcYear" %in% names(controls_reward_calculation))){
      controls_reward_calculation <- dplyr::cross_join(controls_reward_calculation,
                                                       data.frame(mcYear=mcyears))
    }

    # controls_reward_calculation <- rbind(constraint_values,controls_reward_calculation) %>%
    #   dplyr::select(-c("sim")) %>%
    #   dplyr::distinct() %>%
    #   dplyr::arrange(.data$week,.data$u)

    area_here <- area

    constraint_values <- constraint_values %>%
      dplyr::filter(area==area_here) %>%
      dplyr::select(-c("area"))

    reward_db <- antaresWaterValues::get_Reward(simulation_values = constraint_values,
                                                simulation_names = simulation_names,
                                                opts=opts,
                                                area = area, mcyears = mcyears,
                                                method_old = F, max_hydro = max_hydro,
                                                possible_controls =controls_reward_calculation,
                                                expansion = T,
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
#' @param final_level Numeric in [0, 100]. Final and initial level in percentage of H2 storage.
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
                            final_level) {

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

  if (all(new_rewards$reward$control == 0)) {print("cluster bande trop grand")}

  # call grid_matrix to compute lb
  res <- antaresWaterValues::Grid_Matrix(area = area,
                        reward_db = new_rewards,
                        mcyears = mc_years,
                        states_step_ratio = 1/51,
                        reservoir_capacity = storage_vol,
                        cvar_value = cvar,
                        opts = opts,
                        penalty_low = penalty_low,
                        penalty_high = penalty_high,
                        force_final_level = T,
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
#' @param mcYear Vector of integers. Monte Carlo years of the rewards.
#'
#' @returns a \code{list} of rewards and decision space, like \code{reward_init}.
update_reward_cluster_flexible <- function(power, marg_cost, reward_init, mcYear) {
  old_reward <- reward_init$reward
  new_reward <- data.frame()

  for (year in mcYear) {
    for (week in 1:max(old_reward$timeId)) {
      #separate each week from another
      reward_week <- subset(subset(old_reward, .data$timeId == week), mcYear == year)

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
          }
          power_max <- min(power_max, power*168)
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
  return(subset(grid_costs[best_index,], select = -.data$Total_cost))
}


#' Updates the candidates bounds at the end of an iteration.
#'
#' @param best_candidate _
#' @param candidates_data List of vector of doubles of length 3. One vector of double for each cluster candidate.
#' The vectors of doubles have the form (bound min, bound max, number of points).
#' The number of points must be >3 to update bounds at each iterations and approach solution.
#' @param storage_points Vector of integers. Storage volumes candidates at these iteration
#'
#' @returns A \code{list} with : storage_bounds (Vector of integers. Storage volumes candidates at next iteration.) and candidates_data (List. It is of the same form as the \code{candidates_data} argument with new clusters candidates.)
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
#' @param storage_final_level Numeric in [0, 100]. Final and initial level in percentage of H2 storage.
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
                                        storage_final_level) {

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
                                      final_level = storage_final_level)
    print(storage_vol)
    print(new_candidate_grid[[candidate_index]])
    print(total_cost_can)
    new_grid_costs <- rbind(new_grid_costs,
                        c(storage_vol, new_candidate_grid[[candidate_index]], total_cost_can))
  }
  colnames(new_grid_costs) <- c("Storage", candidates_types$name, "Total_cost")
  return(new_grid_costs)
}







#' Calculate an optimal trajectory for the reservoir levels based on water values
#' taking into account the mean inflow,
#' used in \code{calculateBellmanWithIterativeSimulations}
#'
#' @param level_init Initial level of the reservoir in MWh
#' @param watervalues Data frame aggregated watervalues generated by \code{Grid_Matrix}
#' @param mcyears Vector of monte carlo years used to evaluate rewards
#' @param controls Data frame containing possible transition for each week,
#' generated by the function \code{constraint_generator}
#' @param niveau_max Capacity of the reservoir in MWh
#' @param penalty_low Penalty for violating the bottom rule curve
#' @param penalty_high Penalty for violating the top rule curve
#' @param max_hydro_weekly Data frame with weekly maximum pumping and generating powers
#' @param pump_eff Pumping efficiency (1 if no pumping)
#' @param penalty_final_level Penalty for final level
#' @param final_level Final level
#' @param mix_scenario Should scenario be mix from one week to another ?
#'
#' @return Data frame with level (lev) and transition
#' to evaluate (constraint) for each week (w)
getOptimalTrend_var <- function(level_init,watervalues,mcyears,controls,
                            niveau_max,penalty_low,penalty_high,
                            penalty_final_level, final_level,
                            max_hydro_weekly, pump_eff,mix_scenario=TRUE){
  level_i <- data.frame(states = level_init,scenario = seq_along(mcyears))
  levels <- data.frame()

  set.seed(0) # just to make it reproducible

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


  levels <- levels %>%
    dplyr::mutate(true_constraint = .data$constraint)

  return(levels)
}

