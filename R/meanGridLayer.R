#' Calculate grid mean layer matrix
#'
#' @param area An 'antares' area.
#' @param simulation_names Names of simulations to retrieve
#' @param simulation_values Values for the simulation.
#' @param states States matrix.
#' @param max_mcyears Number of MC years to consider, by default all of them.
#' @param n_week Number of weeks.
#' @param week_53 Water values for week 53, by default 0.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}  
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom antaresRead readInputTS
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
meanGridLayer <- function(area, simulation_names, simulation_values = NULL, states, max_mcyears = NULL, n_week = 53, week_53 = 0, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  assertthat::assert_that(ncol(states) == n_week)
  
  # max hydro (E_max, fr = 1.344)
  monthly_hydroStorage <- antaresRead::readInputTS(hydroStorageMaxPower = area, timeStep = "hourly", opts = opts)
  max_hydro <- monthly_hydroStorage[, max(hstorPMaxHigh)]*168/1e6
  if (is.null(simulation_values)) {
    simulation_values <- seq(from = 0, to = max_hydro, length.out = length(simulation_names))
    message(paste0("Using simulation_values: ", paste(simulation_values, collapse = ", ")))
  }
  
  if (is.null(max_mcyears))
    max_mcyears <- opts$parameters$general$nbyears
  max_mcyears <- seq_len(max_mcyears)
  
  rev_week <- rev(seq_len(n_week))
  
  if (length(week_53) == 1)
    week_53 <- rep_len(week_53, nrow(states))
  
  value_node  <- matrix(data = NA, nrow = nrow(states), ncol = n_week - 1)
  value_node <- cbind(value_node, week_53)
  
  # decision space
  decision_space <- matrix(data = simulation_values, nrow = length(simulation_values), ncol = n_week)
  
  # inflow
  inflow <- antaresRead::readInputTS(hydroStorage = area, timeStep = "weekly", opts = opts)
  inflow <- inflow[, hydroStorage := hydroStorage / 1e6]
  
  # decalage des donnees
  decalage <- data.frame(timeId = seq_len(53), timeIdNew = c(26:53, 1:25))
  # decalage$timeIdNew <- decalage$timeId + 25
  # decalage$timeIdNew[decalage$timeIdNew>52] <- decalage$timeIdNew[decalage$timeIdNew>52]-52
  # decalage$timeIdNew <- as.integer(decalage$timeIdNew)
  inflow <- merge(x = inflow, y = decalage, by = "timeId")
  inflow <- inflow[, timeId := NULL]
  setnames(x = inflow, old = "timeIdNew", new = "timeId")
  
  
  # Reward
  reward <- getReward(simulation_names = simulation_names, opts = opts)
  
  # Reservoir (calque)
  reservoir <- readReservoirLevels(area, timeStep = "weekly", opts = opts)
  
  # preparation donnees
  watervalues <- data.table(expand.grid(weeks = seq_len(n_week), years = max_mcyears))
  
  # add states
  statesdt <- as.data.table(states)
  statesdt <- melt(data = statesdt, measure.vars = seq_len(n_week), variable.name = "weeks", value.name = "states")
  statesdt <- statesdt[, weeks := as.numeric(gsub("V", "", weeks))]
  statesdt <- statesdt[, statesid := seq_along(states), by = weeks]
  
  # add states plus 1
  statesplus1 <- copy(statesdt)
  statesplus1 <- statesplus1[, weeks := weeks + 1]
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
  
  # add decision space
  decision_space <- as.data.table(decision_space)
  decision_space <- melt(data = decision_space, measure.vars = seq_len(n_week), variable.name = "weeks", value.name = "decision_space")
  decision_space <- decision_space[, weeks := as.numeric(gsub("V", "", weeks))]
  decision_space <- decision_space[, list(decision_space = list(unlist(decision_space))), by = weeks]
  watervalues <- merge(x = watervalues, y = decision_space, by = "weeks", all = TRUE)
  
  # Calcul by week
  next_week_values <- week_53
  for (i in 52:1) {
    
    res_wv <- watervalues[
      weeks == i,
      value_node := calculate_value_node(
        states = states, states_next = states_next, value_reward = reward,
        value_inflow = hydroStorage, decision_space = decision_space, level_high = level_high, 
        level_low = level_low, value_node_next_week = next_week_values, niveau_max = max(states), E_max = max_hydro
      ),
      by = list(years, statesid)
      ]
    
    next_week_values <- res_wv[weeks == i, list(value_node_year = mean(value_node, na.rm = TRUE)), by = statesid][, c(value_node_year)]
    
  }
  
  value_nodes_dt <- res_wv[, list(value_node = mean(value_node, na.rm = TRUE)), by = list(weeks, statesid)]
  
  # value_nodes_dc <- dcast(data = value_nodes_dt, formula = statesid ~ weeks, value.var = "value_node")
  
  # add states levels
  value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))
  
  return(value_nodes_dt)
}



# TODO : documentation

calculate_value_node <- function(states, states_next, value_reward, value_inflow, decision_space, level_high, level_low, value_node_next_week, niveau_max = 10, E_max = 1.344) {
  
  value_reward <- unlist(value_reward)
  decision_space <- unlist(decision_space)
  states_next <- unlist(states_next)
  
  alpha <- 1e-5
  
  # value_node_year <- rep(NA_real_, times = length(states))
  # 
  if (states >= level_high - alpha) {
    return(-Inf)
  }
  if (states <= level_low + alpha) {
    return(-Inf)
  }
  
  
  largest_decision <- min(c(states + value_inflow, E_max), na.rm = TRUE)
  
  decisions_current_benef <- decision_space[decision_space <= largest_decision + alpha]
  
  provisional_steps <- unique(c(decisions_current_benef, E_max) )
  provisional_reward_line <- unique(c(value_reward[seq_along(decisions_current_benef)] , value_reward[length(value_reward)]))
  
  next_states <- states_next[states_next >= (states - E_max + value_inflow) & states_next <= (states + value_inflow + alpha) ]
  
  decisions_benef_to_go <- states - next_states + value_inflow
  
  decisions <- unique(sort(c(decisions_benef_to_go, decisions_current_benef), decreasing = FALSE))
  
  if (!any(decisions_benef_to_go %in% decisions_current_benef)) {
    
    # boucle sur ?
    for (index in setdiff(decisions_benef_to_go, decisions_current_benef)) { # index <- 0.008604931
      
      before <- provisional_steps[index >= provisional_steps - alpha]
      before <- before[length(before)]
      
      after <- provisional_steps[index <= provisional_steps + alpha]
      after <- after[1]
      
      remainder <- (index -  before) / (after - before)
      remainder <- abs(remainder - trunc(remainder)) 
      
      index_before <- match(before, provisional_steps)
      index_after <- match(after,provisional_steps)
      
      interpolation_current_benef <- provisional_reward_line[index_before]*(1-remainder) + provisional_reward_line[index_after]*remainder
      
      
      provisional_steps <- unique(sort(c(index, provisional_steps), decreasing = FALSE))
      
      new_reward_element <- interpolation_current_benef
      
      
      
      provisional_reward_line <- c(provisional_reward_line[seq_len(index_before)], new_reward_element, provisional_reward_line[index_after:length(provisional_reward_line)])
      
    } # fin boucle sur ?
    
  } # fin if
  
  
  decisions <- decisions[decisions - alpha <= states + value_inflow - level_low]
  
  decisions <- decisions[decisions + alpha >= states + value_inflow - level_high]
  
  
  
  temp <- vector(mode = "numeric", length = length(decisions))
  count_x <- 0
  
  for(l in decisions) { # l <- 0
    
    count_x <- count_x + 1
    
    if ((states - l + value_inflow) >= niveau_max + alpha) {
      next
    }
    
    states_above <- states_next[states_next >= (states - l + value_inflow) - alpha]
    states_below <- states_next[states_next <= (states - l + value_inflow) + alpha]
    
    next_node_up <- match(states_above[length(states_above)], states_next, nomatch = 0) 
    next_node_down <- match(states_below[1], states_next, nomatch = 0)
    
    remainder <- 0
    
    if (isTRUE(next_node_up != next_node_down)) {
      remainder <- (states - l + value_inflow) %% (states_above[length(states_above)] - states_below[1]) 
    }
    
    interpolation <- remainder * value_node_next_week[next_node_up] + (1 - remainder) * value_node_next_week[next_node_down]
    
    temp[count_x] <- sum(c(provisional_reward_line[match(l, provisional_steps)], interpolation), na.rm = TRUE)
    
  }
  
  max(temp, na.rm = TRUE)
}


