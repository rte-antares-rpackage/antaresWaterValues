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
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param storage_annual_cost Numeric. Annual cost of storage in eur/MWh.
#' @param parallelprocess Boolean. True to compute Water values with parallel processing.
#' @param nb_sockets Integer. Number of sockets for parallel processing
#' @param unspil_cost Numeric. Unspilled energy cost in eur/MW for all concerned areas.
#' @param back_to_first_node Boolean. True to play again first node at the end. There is no possibility to go uninvest.
#' @param nb_sims Integer. Number of simulations to launch to evaluate reward.
#' @param file_intermediate_results Character. Local path to save intermediate results.
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
                                                         cvar=1,
                                                         storage_annual_cost,
                                                         nb_sims = 51,
                                                         parallelprocess = F,
                                                         nb_sockets = 0,
                                                         unspil_cost = 3000,
                                                         file_intermediate_results,
                                                         back_to_first_node = F) {

  # initialization
  storage_bounds_init <- storage_bounds

  list_efficiency <- c()
  list_max_hydro_weekly = list()
  for (i in 1:length(areas_invest)){
    list_max_hydro_weekly[[areas_invest[[i]]]] = get_max_hydro(areas_invest[[i]],opts,timeStep = "weekly")
    list_efficiency <- c(list_efficiency,getPumpEfficiency(area=areas_invest[i],opts=opts))
  }
  names(list_efficiency) <- areas_invest

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

  optimal_traj = data.frame()
  list_inflow = list()
  for (j in 1:length(areas_invest)){
    list_inflow[[areas_invest[[j]]]] =  get_inflow(area=areas_invest[[j]], opts=opts,mcyears=mc_years_optim)

    optimal_traj <- list_inflow[[areas_invest[[j]]]] %>%
      dplyr::filter(.data$tsId %in% mc_years_optim, .data$timeId<=52) %>%
      dplyr::left_join(list_max_hydro_weekly[[areas_invest[[j]]]],by = dplyr::join_by("timeId")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hydroStorage = .data$hydroStorage) %>%
      dplyr::select(c("timeId","tsId","hydroStorage")) %>%
      dplyr::rename("u"="hydroStorage","week"="timeId","mcYear"="tsId") %>%
      dplyr::mutate(area=areas_invest[[j]]) %>%
      dplyr::ungroup() %>%
      rbind(optimal_traj)
  }

  if (file.exists(file_intermediate_results)){
    load(file = file_intermediate_results)
  } else {
    output_node <- list()
  }

  for (node in areas_loop) {
    time1 <- Sys.time()
    nb_ite <- 0

    if (!(node %in% names(output_node))){
      output_node[[node]] <- list()
    }

    candidates_types <- candidates_types_gen%>% dplyr::filter(.data$Zone==node)

    grid_costs <- data.frame(matrix(ncol = 3+length(candidates_types$index), nrow=0))

    storage_bounds <- storage_bounds_init

    candidates_data <- c()
    for (can in 1:length(candidates_types$index)) {
      candidates_data[[can]] <- c(as.numeric(candidates_types$Borne_min[can]), as.numeric(candidates_types$Borne_max[can]),
                                  as.numeric(candidates_types$Points_nb[can]))
    }

    if ("reward" %in% names(output_node[[node]])){
      reward_db = output_node[[node]]$reward
    } else {
      reward_db <-
        calculateRewardsSimulations(node =node,
                                    list_areas = areas_invest,
                                    list_efficiency = list_efficiency,
                                    opts = opts,
                                    mcyears = mc_years_optim,
                                    prefix=paste0("unsp", as.character(unspil_cost)),
                                    sim_number = nb_sims,
                                    optimal_traj = optimal_traj,
                                    candidates_types = candidates_types,
                                    list_max_hydro_weekly = list_max_hydro_weekly)


      # store results edited by area index
      output_node[[node]]$reward <- reward_db

      save(output_node,file=file_intermediate_results)
    }

    if (!("optimal_traj" %in% names(output_node[[node]]))){

      final_level <- get_initial_level(node,opts)

      # loop on the candidates grids
      while (nb_ite < max_ite) {
        nb_ite <- nb_ite + 1

        # compute storage volumes to evaluate
        new_storage_points <- c(seq(storage_bounds[1], storage_bounds[2], length.out = storage_points_nb))
        for (sto in 1:length(new_storage_points)) {new_storage_points[sto] <- as.integer(new_storage_points[sto])}

        # compute grid of the other candidates
        new_candidate_grid <- grid_other_candidates(candidates_data)

        previous_candidate = as.matrix(grid_costs)[,2:(ncol(grid_costs)-2)]
        to_remove = rep(F,length(new_candidate_grid))
        # add fixed part of flexible cluster to candidates
        for (i in 1:length(new_candidate_grid)) {
          for (j in 1:length(candidates_types$index)) {
            if (candidates_types$Part_fixe[j] > 0) {
              new_candidate_grid[[i]] <- c(new_candidate_grid[[i]], new_candidate_grid[[i]][as.integer(candidates_types$index[j])]*as.numeric(candidates_types$Part_fixe[j]))}
          }
          if (all(new_candidate_grid[[i]] %in% previous_candidate)){
            to_remove[[i]] = TRUE
          }
        }
        new_candidate_grid = new_candidate_grid[!to_remove]

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
                                          "storage_annual_cost",
                                          "Grid_Matrix",
                                          "getPumpEfficiency",
                                          "%>%",
                                          "data.table",
                                          "readReservoirLevels",
                                          "fread_antares",
                                          "isRunning",
                                          "fread",
                                          "as.data.table",
                                          "melt",
                                          "copy",
                                          "Bellman",
                                          "setDT",
                                          "build_all_possible_decisions",
                                          "build_data_watervalues",
                                          "value_node_gen",
                                          "get_initial_level",
                                          "get_initial_level_year_per_year",
                                          "getOptimalTrend"))


            list_index <- c(seq(1, length(new_candidate_grid)))
            print("begin parallel processing")
            print(storage_vol)
            # compute WV with parallel processing
            new_grid_costs <- parallel::parLapply(cl, list_index, function(x) {
              total_cost_parallel_version(x,
                                          new_candidate_grid,
                                          node,
                                          mc_years_optim,
                                          candidates_types,
                                          storage_vol,
                                          reward_db,
                                          cvar,
                                          opts,
                                          penalty_low,
                                          penalty_high,
                                          penalty_final_level,
                                          storage_annual_cost,
                                          final_level,
                                          list_max_hydro_weekly[[node]],
                                          list_inflow[[node]])
            })
            parallel::stopCluster(cl)

            for (cost in new_grid_costs) {grid_costs <- rbind(grid_costs, cost)}
          } else {
            # without parallel processing
            # loop on the other candidates
            for (candidate_index in 1:length(new_candidate_grid)) {
              # calculate total cost
              total_cost_can <- total_cost_loop(area = node,
                                                mc_years = mc_years_optim,
                                                candidate_pool = new_candidate_grid[[candidate_index]],
                                                candidates_types = candidates_types,
                                                storage_vol = storage_vol,
                                                df_reward = reward_db,
                                                cvar = cvar,
                                                opts = opts,
                                                penalty_low = penalty_low,
                                                penalty_high = penalty_high,
                                                penalty_final_level = penalty_final_level,
                                                storage_annual_cost = storage_annual_cost,
                                                final_level = final_level,
                                                max_hydro_weekly = list_max_hydro_weekly[[node]],
                                                inflow = list_inflow[[node]])
              print(storage_vol)
              print(new_candidate_grid[[candidate_index]])
              print(total_cost_can$total_cost)
              print(total_cost_can$op_cost)
              grid_costs <- rbind(grid_costs,
                                  c(storage_vol, new_candidate_grid[[candidate_index]], total_cost_can$total_cost, total_cost_can$op_cost))
            }
          }
        }

        # find the best candidate at this iteration
        colnames(grid_costs) <- c("Storage", candidates_types$name, "Total_cost","op_cost")
        best_candidates <- max_candidate(grid_costs)

        # update bounds
        bounds <- new_bounds(best_candidates, candidates_data, new_storage_points)
        candidates_data <- bounds$candidates_data
        storage_bounds <- bounds$storage_bounds
      }

      # store output
      output_node[[node]]$all_costs <- grid_costs
      output_node[[node]]$best <- max_candidate(grid_costs)
      output_node[[node]]$last_storage_points <- new_storage_points
      output_node[[node]]$last_candidates_data <- candidates_data

      time2 <- Sys.time()
      output_node[[node]]$optim_time <- time2-time1

      print("Best for this node is :")
      print(output_node[[node]]$best)

      res = total_cost_loop(area = node,
                            mc_years = mc_years_optim,
                            candidate_pool = unlist(dplyr::select(output_node[[node]]$best,-c("Storage")),use.names = F),
                            candidates_types = candidates_types,
                            storage_vol = unlist(dplyr::select(output_node[[node]]$best,c("Storage")),use.names = F),
                            df_reward = reward_db,
                            cvar = cvar,
                            opts = opts,
                            penalty_low = penalty_low,
                            penalty_high = penalty_high,
                            penalty_final_level = penalty_final_level,
                            storage_annual_cost = storage_annual_cost,
                            final_level = final_level,
                            compute_optimal_traj = T,
                            max_hydro_weekly = list_max_hydro_weekly[[node]],
                            inflow = list_inflow[[node]])
      optimal_traj <- optimal_traj %>%
        dplyr::filter(.data$area != node) %>%
        rbind(res$optimal_traj)

      output_node[[node]]$optimal_traj = res$optimal_traj
      output_node[[node]]$watervalues = to_Antares_Format(res$watervalues)

      # store rewards and costs in a file
      save(output_node,file=file_intermediate_results)

      # add best clusters to node in the study and edit storage size
      l_old_clusters <- c()
      l_read_clusters <- antaresRead::readClusterDesc(opts=opts)
      l_all_clusters <- as.character(l_read_clusters$cluster)

      antaresEditObject::writeIniHydro(area = node, params = c("reservoir capacity" = output_node[[node]][["best"]][["Storage"]]), opts = opts)

      antaresEditObject::writeWaterValues(area = node, data = output_node[[node]]$watervalues, opts=opts)

      for (i in 1:length(l_all_clusters)) {if (l_read_clusters$area[i]==node) {l_old_clusters <- c(l_old_clusters, l_all_clusters[i])}}

      for (can in 1:length(candidates_types$index) ){
        power <- output_node[[node]][["best"]][[candidates_types$name[can]]]
        ts_avail <- matrix(power, nrow = 8760, ncol = 1)

        if (paste0(node, "_", candidates_types$name[can]) %in% l_old_clusters) {
          old_power <- antaresRead::readClusterDesc(opts) %>%
            dplyr::filter(.data$cluster == paste0(node, "_", candidates_types$name[can])) %>%
            dplyr::pull(c("nominalcapacity"))
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
#' Called for each area of \code{MultiStock_H2_Investment_reward_compute_once}.
#' If they are several areas, the trajectories of the other storage are fixed on their optimal trend.
#'
#' @param node Character. Name of the area where the reward is computed
#' @param list_areas Vector of characters of the names of areas to optimize.
#' @param list_efficiency Vector of numeric with pumping efficiency for the area with corresponding index in \code{list_areas}.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param mcyears Vector of integers. Monte Carlo years to run simulations
#' @param prefix Character. Prefix of the simulation.
#' @param sim_number Integer. Number of simulations.
#' @param optimal_traj Data frame containing optimal trajectory for all areas
#' @param list_max_hydro_weekly List of data.frame. Generated by \code{get_max_hydro()} for each area.
#' @param candidates_types Data_frame with column names : c("index", "name", "type", "TOTEX", "Marg_price").
#'
#' @returns a \code{data_frame} containing the rewards returned by the function \code{antaresWaterValues::get_Reward()}
calculateRewardsSimulations <- function(node,
                                        list_areas,
                                        list_efficiency,
                                        opts,
                                        mcyears,
                                        prefix,
                                        sim_number = 5,
                                        optimal_traj,
                                        candidates_types,
                                        list_max_hydro_weekly)  {

  list_areas = tolower(list_areas)

  list_backup = list()
  for (j in seq_along(list_areas)){
    area = list_areas[[j]]
    check_area_name(area = area, opts = opts)

    list_backup[[j]] = getBackupData(area,mcyears,opts)
  }

  max_hydro_daily = fread_antares(opts, file = file.path(opts$inputPath, "hydro", "common", "capacity", paste0("maxpower_",node,".txt")))

  year_by_year = opts$parameters$general$`year-by-year`
  synthesis = opts$parameters$output$synthesis
  storenewset = opts$parameters$output$storenewset

  assertthat::assert_that(is_api_study(opts),msg="Study must be in API mode to launch simulations.")

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

    data = max_hydro_daily
    data[,1] = data[,1] + sum(as.double(candidates_types$Borne_max))
    antaresEditObject::writeHydroValues(node,
                                        type = "maxpower",
                                        data=data,
                                        opts = opts)

    grid = data.frame()
    for (j in seq_along(list_areas)){
      a <- list_areas[[j]]
      if (a != node){
        grid = optimal_traj %>%
          dplyr::filter(.data$area == a) %>%
          dplyr::left_join(list_max_hydro_weekly[[j]],by=c("week"="timeId")) %>%
          dplyr::mutate(rhs = (u+pump*list_efficiency[[j]])/(turb+pump*list_efficiency[[j]])) %>%
          dplyr::mutate(grid_id = 0,
                        problem_name = stringr::str_c("problem-",mcYear,"-",week,"--optim-nb-1"),
                        type = "constraint",
                        name = "HydroPower",
                        min = rhs,
                        max = rhs,
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
                          step = 1/(sim_number-1)) %>%
          rbind(grid)
      }

    }

    if(!is_api_study(opts)){
      write.csv(rows,file=paste0(study_path,"/grid.csv"),row.names = F, quote = F)
    } else {
      body = list()
      tc <- textConnection("out", "w")
      write.csv(grid, tc, row.names = FALSE, quote = FALSE)
      close(tc)

      body$file <- paste(out, collapse = "\n")
      antaresRead::api_put(opts=opts,endpoint=paste0(opts$study_id,
                                                     "/raw?path=user%2Fgrid.csv&create_missing=true&resource_type=file"),
                           body=body)
    }

    # Start the simulations

    name_sim = paste0(prefix,"_grid_cost_function_",node)

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

    if (i==max_try){ assertthat::assert_that(1==0)}

    zipfile <- tempfile(fileext = ".zip")
    tmpdir  <- tempdir()
    writeBin(download_res, zipfile)
    unzip(zipfile, files = "gridPointsValues_0.csv", exdir = tmpdir)

    reward = read.csv(paste0(tmpdir,"/gridPointsValues_0.csv"))

    unlink(zipfile)
    unlink(tmpdir, recursive = TRUE)

    reward = reward %>%
      dplyr::mutate(timeId = .data$week,
                    mcYear = .data$scenario,
                    control = .data[[paste0(node,"_RHSValue")]],
                    reward = -cost) %>%
      dplyr::select(c("timeId","mcYear","control","reward"))
    assertthat::assert_that(sum(is.na(reward))==0)
    reward_db = list()
    reward_db$reward = reward
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
    antaresEditObject::writeHydroValues(node,
                                        type = "maxpower",
                                        data=max_hydro_daily,
                                        opts = opts)
    antaresEditObject::updateGeneralSettings(year.by.year = year_by_year, opts=opts)
    antaresEditObject::updateOutputSettings(synthesis = synthesis, storenewset = storenewset, opts=opts)
  }
  )

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
#' @param compute_optimal_traj Boolean.
#' @param max_hydro_weekly Data.frame. Generated by \code{get_max_hydro()}
#' @param inflow Generated by \code{get_inflow()}
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
                            compute_optimal_traj = F,
                            max_hydro_weekly,
                            inflow) {

  for (can in 1:length(candidate_pool)) {candidate_pool[can] <- as.integer(candidate_pool[can])}

  new_rewards <- df_reward

  # change reward function to include other candidates
  if (!is.null(candidates_types)) {
    for (can in 1:length(candidates_types$index)) {
      index <- candidates_types$index[can]
      type <- candidates_types$type[can]

      # update reward function following the type of the candidate
      if (type == "cluster bande" & length(new_rewards$reward$timeId) > 0 & candidate_pool[can] > 0) {
        new_rewards <- update_reward_cluster_bande(candidate_pool[can], new_rewards)
        print(paste0(candidates_types$name[can], " updated"))
      }
      if (type == "cluster flexible"  & length(new_rewards$reward$timeId) > 0 & candidate_pool[can] > 0) {
        new_rewards <- update_reward_cluster_flexible(candidate_pool[can], as.numeric(candidates_types$Marg_price[can]), new_rewards, mc_years)
        print(paste0(candidates_types$name[can], " updated"))
      }
    }
  }

  # call grid_matrix to compute lb
  res <- Grid_Matrix(area = area,
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
                     penalty_final_level_low = penalty_final_level,
                     plot_watervalues = F,
                     max_hydro_weekly = max_hydro_weekly,
                     inflow = inflow)

  op_cost <- res$lower_bound

  # compute total cost
  total_cost <- op_cost - storage_annual_cost*storage_vol
  for (can in 1:length(candidates_types$index)) {
    total_cost <- total_cost - as.numeric(candidates_types$TOTEX[can])*candidate_pool[can]
    if (candidates_types$type[can] == "cluster bande") {
      total_cost <- total_cost - as.numeric(candidates_types$Marg_price[can])*candidate_pool[can]*8736
    }
  }

  if (compute_optimal_traj){
    level_init <- get_initial_level_year_per_year(area,opts)*storage_vol/100

    pump_eff = getPumpEfficiency(area,opts)
    levels <- getOptimalTrend(level_init=level_init,watervalues=res$watervalues,
                              mcyears=mc_years,reward=new_rewards$reward,controls=new_rewards$controls,
                              niveau_max = storage_vol,df_levels = data.frame(),
                              penalty_low = penalty_low, penalty_high = penalty_high,
                              penalty_final_level = penalty_final_level, final_level = final_level,
                              max_hydro_weekly=max_hydro_weekly, n=3,
                              pump_eff = pump_eff, df_previous_cut = NULL, mix_scenario = F)
    levels <- levels %>%
      dplyr::mutate(area = area, u = .data$true_constraint) %>%
      dplyr::select(c("week","mcYear","u","area"))
  } else {
    levels = data.frame()
  }
  output = list()
  output$total_cost = total_cost
  output$op_cost = op_cost
  output$optimal_traj = levels
  output$watervalues = res$aggregated_results
  return(output)
}



#' Modify the reward function to take into account the effects of a must-run cluster.
#'
#' @param power Integer. Capacity of the cluster.
#' @param reward_init List. Rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#'
#' @returns a \code{list} of rewards and decision space, like \code{reward_init}.
update_reward_cluster_bande <- function(power, reward_init) {
  volume_change <- 168*power

  reward_init$reward = reward_init$reward %>%
    dplyr::mutate(control = .data$control - volume_change)

  return(reward_init)
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
      reward_week <- old_reward %>%
        dplyr::filter(.data$timeId == week, mcYear == year)

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
  return(dplyr::select(grid_costs[best_index,],-c("Total_cost","op_cost")))
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
#' @param reward_db List. List of rewards and decision space from the output of the function \code{antaresWaterValues::get_Reward()}.
#' Rewards and decision spaces are given for each area marked by their number.
#' @param cvar Numeric in [0,1]. The probability used in cvar algorithm.
#' @param opts List of study parameters returned by the function \code{antaresRead::setSimulationPath(simulation="input")} in input mode.
#' @param penalty_low Integer. Penalty for lower guide curve.
#' @param penalty_high Integer. Penalty for higher guide curve.
#' @param penalty_final_level Integer. Penalty for higher and lower final level.
#' @param storage_annual_cost Numeric. Total annual cost of the storage candidate in eur/MWh
#' @param storage_final_level Numeric in [0, 100]. Final and initial level in percentage of H2 storage.
#' Must be in adequacy with the step number if the reward function.
#' @param max_hydro_weekly Data.frame. Generated by \code{get_max_hydro()}
#' @param inflow Generated by \code{get_inflow()}
#'
#' @returns The total reward for the specified candidates in euros. Because of the Bellman values it is usually negative and large
total_cost_parallel_version <- function(candidate_index,
                                        new_candidate_grid,
                                        node,
                                        mc_years_optim,
                                        candidates_types,
                                        storage_vol,
                                        reward_db,
                                        cvar,
                                        opts,
                                        penalty_low,
                                        penalty_high,
                                        penalty_final_level,
                                        storage_annual_cost,
                                        storage_final_level,
                                        max_hydro_weekly,
                                        inflow) {
  # calculate total cost
  new_grid_costs <- data.frame(matrix(ncol = 2+length(candidates_types$index), nrow=0))
  total_cost_can <- total_cost_loop(area = node,
                                    mc_years = mc_years_optim,
                                    candidate_pool = new_candidate_grid[[candidate_index]],
                                    candidates_types = candidates_types,
                                    storage_vol = storage_vol,
                                    df_reward = reward_db,
                                    cvar = cvar,
                                    opts = opts,
                                    penalty_low = penalty_low,
                                    penalty_high = penalty_high,
                                    penalty_final_level = penalty_final_level,
                                    storage_annual_cost = storage_annual_cost,
                                    final_level = storage_final_level,
                                    max_hydro_weekly = max_hydro_weekly,
                                    inflow = inflow)
  print(storage_vol)
  print(new_candidate_grid[[candidate_index]])
  print(total_cost_can$total_cost)
  new_grid_costs <- rbind(new_grid_costs,
                          c(storage_vol, new_candidate_grid[[candidate_index]], total_cost_can$total_cost,total_cost_can$op_cost))
  colnames(new_grid_costs) <- c("Storage", candidates_types$name, "Total_cost", "op_cost")
  return(new_grid_costs)
}
