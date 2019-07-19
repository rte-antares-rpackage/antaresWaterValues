#' Calculate grid mean layer matrix
#'
#' @param area An 'antares' area.
#' @param simulation_names Names of simulations to retrieve.
#' @param simulation_values Values for the simulation.
#' @param nb_cycle Number of times to run the algorithm.
#' @param district_name Name of the district used to store output.
#' @param max_mcyears Number of MC years to consider, by default all of them.
#' @param week_53 Water values for week 53, by default 0.
#' @param method Perform mean grid algorithm or grid mean algorithm ?
#' @param states_steps Steps to discretize steps levels between the reservoir capacity and zero.
#' @param reservoir_capacity Reservoir capacity for the given area in GWh, if \code{NULL} (the default),
#'  value in Antares is used if available else a prompt ask the user the value to be used.
#' @param na_rm Remove NAs
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom antaresRead readInputTS
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
meanGridLayer <- function(area, simulation_names, simulation_values = NULL, nb_cycle = 2L,
                          district_name = "water values district", max_mcyears = NULL, 
                          week_53 = 0, method = c("mean-grid", "grid-mean"), 
                          states_steps = 0.05,
                          reservoir_capacity = NULL, na_rm = FALSE, 
                          opts = antaresRead::simOptions()) {
  
  method <- match.arg(method)
  assertthat::assert_that(class(opts) == "simOptions")
  
  # Number of weeks
  n_week <- 52
  
  # assertthat::assert_that(ncol(states) == n_week)
  
  # max hydro (E_max, fr = 1.344)
  monthly_hydroStorage <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  if (hasName(monthly_hydroStorage, "hstorPMaxHigh")) {
    max_hydro <- monthly_hydroStorage[, max(hstorPMaxHigh)] * 168 / 1e6
  } else {
    max_hydro <- monthly_hydroStorage[, max(generatingMaxPower)] * 168 / 1e6
  }
  if (is.null(simulation_values)) {
    simulation_values <- seq(from = 0, to = max_hydro, length.out = length(simulation_names))
    message(paste0("Using simulation_values: ", paste(simulation_values, collapse = ", ")))
  }
  
  if (is.null(max_mcyears))
    max_mcyears <- opts$parameters$general$nbyears
  max_mcyears <- seq_len(max_mcyears)
  
  # Niveau max
  if (is.null(reservoir_capacity)) {
    niveau_max <- getReservoirCapacity(area = "fr")/1e6
    if (length(niveau_max) == 0) {
      ask_niveau_max <- "Failed to retrieve reservoir capacity from Antares, please specify value (in GWh, e.g. 10000 for France):\n"
      niveau_max <- readline(prompt = ask_niveau_max)
      niveau_max <- as.numeric(niveau_max)/1e3
    }
    if (length(niveau_max) == 0) 
      stop("Failed to retrieve reservoir capacity, please specify it explicitly with 'reservoir_capacity'.")
  } else {
    niveau_max <- reservoir_capacity/1e3
  }
  # niveau_max <- max(states)
  # rev_week <- rev(seq_len(n_week))
  
  # States matrix
  states <- matrix( rep(seq(from = niveau_max, to = 0, by = -states_steps), n_week + 1), byrow = FALSE, ncol = n_week + 1)
  
  if (length(week_53) == 1)
    week_53 <- rep_len(week_53, nrow(states))
  
  # value_node  <- matrix(data = NA, nrow = nrow(states), ncol = n_week)
  # value_node <- cbind(value_node, week_53)
  
  # decision space
  # decision_space <- matrix(data = simulation_values, nrow = length(simulation_values), ncol = n_week+1)
  decision_space <- simulation_values
  
  decimals <- 6
  
  # inflow
  # inflow <- antaresRead::readInputTS(hydroStorage = area, timeStep = "weekly", opts = opts)
  # inflow <- inflow[, hydroStorage := round(hydroStorage / 1e6, decimals-4)]
  # inflow <- inflow[, timeId := gsub(pattern = "\\d{4}-w", replacement = "", x = time)]
  # # inflow <- inflow[timeId == 53L, timeId := 1L]
  # inflow <- inflow[, timeId := as.numeric(timeId)]
  # inflow <- inflow[, list(hydroStorage = sum(hydroStorage, na.rm = TRUE)), by = list(area, timeId, tsId)]
  
  tmp_name <- getSimulationNames(pattern = simulation_names[1], opts = opts)[1]
  suppressWarnings({
    tmp_opt <- setSimulationPath(path = opts$studyPath, simulation = tmp_name)
    inflow <- readAntares(areas = area, hydroStorage = TRUE, timeStep = "weekly", mcYears = "all", opts = tmp_opt)
  })
  inflow <- inflow[order(mcYear, timeId)]
  inflow <- inflow[, list(area, tsId = mcYear, timeId, time, hydroStorage)]
  inflow <- inflow[, hydroStorage := round(hydroStorage / 1e6, digits = 2)]
  inflow <- inflow[, timeId := gsub(pattern = "\\d{4}-w", replacement = "", x = time)]
  inflow <- inflow[, timeId := as.numeric(timeId)]
  inflow <- inflow[, list(hydroStorage = sum(hydroStorage, na.rm = TRUE)), by = list(area, timeId, tsId)]
  options("antares" = opts)
  
  # # # decalage des donnees
  # # decalage <- data.frame(timeId = seq_len(53), timeIdNew = c(26:53, 1:25))
  # # # decalage$timeIdNew <- decalage$timeId + 25
  # # # decalage$timeIdNew[decalage$timeIdNew>52] <- decalage$timeIdNew[decalage$timeIdNew>52]-52
  # # # decalage$timeIdNew <- as.integer(decalage$timeIdNew)
  # # inflow <- merge(x = inflow, y = decalage, by = "timeId")
  # # inflow <- inflow[, timeId := NULL]
  # # setnames(x = inflow, old = "timeIdNew", new = "timeId")
  # # inflow <- inflow[order(timeId, tsId)]
  
  
  # Reward
  reward <- getReward(simulation_names = simulation_names, district_name = district_name, opts = opts)
  reward <- reward[timeId %in% seq_len(n_week)]
  
  # Reservoir (calque)
  reservoir <- readReservoirLevels(area, timeStep = "weekly", byReservoirCapacity = FALSE, opts = opts)
  vars <- c("level_low", "level_avg", "level_high")
  reservoir <- reservoir[, 
                         (vars) := lapply(.SD, function(x) {round(x * max(states), decimals)}),
                         .SDcols = vars
                         ]
  
  # preparation donnees
  watervalues <- data.table(expand.grid(weeks = seq_len(n_week+1), years = max_mcyears))
  
  # add states
  statesdt <- as.data.table(states)
  statesdt <- melt(data = statesdt, measure.vars = seq_len(ncol(states)), variable.name = "weeks", value.name = "states")
  statesdt <- statesdt[, weeks := as.numeric(gsub("V", "", weeks))]
  statesdt <- statesdt[, statesid := seq_along(states), by = weeks]
  statesdt <- statesdt[, states := round(states, decimals)]
  
  # add states plus 1
  statesplus1 <- copy(statesdt)
  statesplus1 <- statesplus1[, weeks := weeks - 1]
  statesplus1 <- statesplus1[, list(states_next = list(unlist(states))), by = weeks]
  statesplus1 <- merge(x = statesdt, y = statesplus1, by = c("weeks"), all.x = TRUE)
  watervalues <- merge(x = watervalues, y = statesplus1, by = "weeks", all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  
  # add inflow
  watervalues <- merge(x = watervalues, y = inflow[, list(weeks = timeId, years = tsId, hydroStorage)], by = c("weeks", "years"))
  
  # add rewards
  reward_l <- reward[, list(reward = list(unlist(.SD))), .SDcols = simulation_names, by = list(weeks = timeId, years = mcYear)]
  watervalues <- merge(x = watervalues, y = reward_l, by = c("weeks", "years"))
  
  # add reservoir
  watervalues <- merge(x = watervalues, y = reservoir[, list(weeks = timeId, level_low, level_high)], by = "weeks", all = TRUE)
  
  # # add decision space
  # decision_space <- as.data.table(decision_space)
  # decision_space <- melt(data = decision_space, measure.vars = seq_len(n_week+1), variable.name = "weeks", value.name = "decision_space")
  # decision_space <- decision_space[, weeks := as.numeric(gsub("V", "", weeks))]
  # decision_space <- decision_space[, list(decision_space = list(unlist(decision_space))), by = weeks]
  # watervalues <- merge(x = watervalues, y = decision_space, by = "weeks", all = TRUE)

  # options("antaresWaterValues.before.meanGrid" = watervalues) # to remove
  
  # Calcul by week
  next_week_values <- week_53
  
  max_hydro <- max(decision_space)
  
  watervalues <- watervalues[, value_node := NA_real_]
  
  verif_next_week <- list()
  
  if (method == "mean-grid") {
    funGridMean <- mean
  } else {
    funGridMean <- max
  }
  
  
  for (n_cycl in seq_len(nb_cycle)) {
    
    cat("Calculating value nodes, cycle number:", n_cycl, "\n")
    
    pb <- txtProgressBar(min = 0, max = 51, style = 3)
    
    for (i in rev(seq_len(52))) { # rep(52:1, times = nb_cycle)
      # print(next_week_values)
      watervalues <- watervalues[weeks == i, value_node := NA_real_]
      watervalues <- watervalues[
        weeks == i,
        value_node := calculate_value_node(
          states = states, states_next = states_next, value_reward = reward,
          value_inflow = hydroStorage, decision_space = decision_space, level_high = level_high, 
          level_low = level_low, value_node_next_week = next_week_values, niveau_max = niveau_max, E_max = max_hydro,
          method = method, na_rm = na_rm
        ),
        by = list(years, statesid)
        ]
      
      next_week_values <- watervalues[weeks == i, list(vny = funGridMean(value_node, na.rm = TRUE)), by = statesid][, c(vny)]
      # next_week_values <- remove_outliers(next_week_values)
      # next_week_values[!is.finite(next_week_values) | is.na(next_week_values)] <- 0
      verif_next_week <- c(verif_next_week, list(next_week_values))
      
      setTxtProgressBar(pb = pb, value = 52 - i)
      
    }
    close(pb)
  }
  
  # verif_next_week <<- verif_next_week

  # verif_watervalues2 <<- watervalues
  
  
  # watervalues <- watervalues[, value_node := remove_outliers(value_node)]
  value_nodes_dt <- watervalues[, list(value_node = funGridMean(value_node, na.rm = TRUE)),
                                by = list(weeks, statesid)]
  
  # value_nodes_dc <- dcast(data = value_nodes_dt, formula = statesid ~ weeks, value.var = "value_node")
  
  # add states levels
  value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))
  
  # Calculate Usage values
  value_nodes_dt <- value_nodes_dt[order(weeks, -statesid)]
  value_nodes_dt <- value_nodes_dt[, value_node_dif := c(NA, diff(value_node)), by = weeks]
  value_nodes_dt <- value_nodes_dt[, states_dif := c(NA, diff(states)), by = weeks]
  value_nodes_dt <- value_nodes_dt[, vu := abs(value_node_dif / states_dif / 1e6)]
  
  return(value_nodes_dt)
}



# TODO : documentation



calculate_value_node <- function(states, states_next, value_reward, value_inflow, decision_space, 
                                 level_high, level_low, value_node_next_week, niveau_max = 10, E_max = 1.344, method, na_rm = FALSE) {
  
  value_reward <- unlist(value_reward, use.names = FALSE)
  decision_space <- unlist(decision_space, use.names = FALSE)
  states_next <- unlist(states_next, use.names = FALSE)
  
  alpha <- getOption(x = "antaresWaterValues.alpha", default = 0.0001)
  decimals <- getOption(x = "antaresWaterValues.decimals", default = 3)
  
  # value_node_year <- rep(NA_real_, times = length(states))
  # 
  if (states >= round(level_high, decimals) - alpha) {
    return(-Inf)
  }
  if (states <= round(level_low, decimals) + alpha) {
    return(-Inf)
  }
  
  
  largest_decision <- min(c(states + value_inflow, E_max), na.rm = TRUE)
  
  largest_decision <- round(largest_decision, decimals) ###
  decisions_current_benef <- decision_space[decision_space <= largest_decision + alpha]
  decisions_current_benef <- round(decisions_current_benef, decimals) ###
  
  provisional_steps <- unique(c(decisions_current_benef, E_max) )
  provisional_reward_line <- unique(c(value_reward[seq_along(decisions_current_benef)] , value_reward[length(value_reward)]))
  
  next_states <- states_next[states_next >= (states - E_max + value_inflow) & states_next <= (states + value_inflow + alpha) ]
  
  decisions_benef_to_go <- states - next_states + value_inflow
  decisions_benef_to_go <- round(decisions_benef_to_go, decimals) ###
  
  decisions <- unique(sort(c(decisions_benef_to_go, decisions_current_benef), decreasing = FALSE))
  
  # if (any(!decisions_benef_to_go %in% decisions_current_benef)) {
  if (length(setdiff(decisions_benef_to_go, decisions_current_benef)) > 0) {
    
    # boucle sur ?
    for (index in setdiff(decisions_benef_to_go, decisions_current_benef)) { # index <- 0.008604931
      
      before <- provisional_steps[index >= provisional_steps - alpha]
      before <- before[length(before)]
      
      after <- provisional_steps[index <= provisional_steps + alpha]
      after <- after[1]
      
      remainder <- (index -  before ) / (after - before)
      remainder <- round(remainder, decimals) ###
      remainder <- abs(remainder - trunc(remainder)) 
      
      # index_before <- match(before, provisional_steps)
      index_before <- which(num_equal(before, provisional_steps))
      index_before <- round(index_before)
      # index_after <- match(after, provisional_steps)
      index_after <- which(num_equal(after, provisional_steps))
      index_after <- round(index_after)
      
      interpolation_current_benef <- provisional_reward_line[index_before]*(1-remainder) + provisional_reward_line[index_after]*remainder
      
      
      provisional_steps <- unique(sort(c(index, provisional_steps), decreasing = FALSE))
      
      new_reward_element <- interpolation_current_benef
      
      
      
      provisional_reward_line <- c(provisional_reward_line[seq_len(index_before)], 
                                   new_reward_element, provisional_reward_line[index_after:length(provisional_reward_line)])
      
    } # fin boucle sur ?
    
  } # fin if
  
  
  decisions <- decisions[decisions - alpha <= states + value_inflow - level_low]
  
  decisions <- decisions[decisions + alpha >= states + value_inflow - level_high]

  decisions <- round(decisions, decimals) ###
  
  
  temp <- vector(mode = "numeric", length = length(decisions))
  count_x <- 0
  
  for(l in decisions) { # l <- 0
    
    count_x <- count_x + 1
    
    if ((states - l + value_inflow) >= niveau_max + alpha) {
      next
    }
    
    states_above <- states_next[states_next >= (states - l + value_inflow) - alpha]
    states_below <- states_next[states_next <= (states - l + value_inflow) + alpha]
    
    next_node_up <- which(num_equal(states_above[length(states_above)], states_next))
    next_node_down <- which(num_equal(states_below[1], states_next))
    
    remainder <- 0
    
    if (!num_equal(next_node_up, next_node_down)) {
      remainder <- (states - l + value_inflow) %% (states_above[length(states_above)] - states_below[1]) 
    } else {
      remainder <- 0
    }
    
    vunw <- value_node_next_week[next_node_up]
    # if (!is.finite(vunw))
    #   vunw <- 0
    vdnw <- value_node_next_week[next_node_down]
    # if (!is.finite(vdnw))
    #   vdnw <- 0
    interpolation <- remainder * vunw + (1 - remainder) * vdnw

    # verif <<- list(
    #   remainder = remainder, next_node_up = next_node_up, next_node_down = next_node_down, states_above = states_above,
    #   states_next = states_next, states_below = states_below, value_node_next_week = value_node_next_week
    # )
    # if (!is.finite(interpolation))
    #   stop()
    
    # temp[count_x] <- sum(c(provisional_reward_line[match(l, provisional_steps)], interpolation), na.rm = na_rm)
    temp[count_x] <- sum(c(provisional_reward_line[num_equal(l, provisional_steps)], interpolation), na.rm = na_rm)
  }
  
  if (method == "mean-grid") {
    max(temp, na.rm = TRUE)
  } else {
    mean_finite(temp, na.rm = TRUE)
  }
}


