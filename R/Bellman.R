Bellman <- function(Data_week,next_week_values_l,decision_space,E_max,niveau_max,method,na_rm=TRUE,max_mcyear,print_test=FALSE,j,...){


  # delet this after testing
  # decision_space <-simulation_res$simulation_values
  # E_max <- max(decision_space)
  # niveau_max <- 90000
  #
  # na_rm <- TRUE
  # method <- "mean-grid"

  decision_space <- unlist(decision_space, use.names = FALSE)
  alpha <- getOption(x = "antaresWaterValues.alpha", default = 0.0001)
  decimals <- getOption(x = "antaresWaterValues.decimals", default = 3)

  for (i in (1:nrow(Data_week))){

    #init
    {
      states <- Data_week$states[i]
      level_high <- Data_week$level_high[i]
      level_low <- Data_week$level_low[i]
      value_inflow <- Data_week$hydroStorage[i]
      reward <- Data_week$reward[[i]]
      states_next <- Data_week$states_next[[i]]


      value_reward <- unlist(reward, use.names = FALSE)
      states_next <- unlist(states_next, use.names = FALSE)


      if (i%% (nrow(Data_week)/max_mcyear) ==1){
        next_week_values <- next_week_values_l[i:(i+(nrow(Data_week)/max_mcyear))]
      }
       }



  #eliminate non accessible states
  if (states > round(level_high, decimals) - alpha) {
    Data_week$value_node[i] <- -Inf
    next
  }
  if (states < round(level_low, decimals) + alpha) {
    Data_week$value_node[i] <- -Inf
    next
  }


  # max possible decision --------
  largest_decision <- min(c(states + value_inflow, E_max,niveau_max), na.rm = TRUE)
  largest_decision <- round(largest_decision, decimals) ###

  # the decisions that respect the max possible decision used in simulation constraints
  decisions_current_benef <- decision_space[decision_space <= largest_decision + alpha]
  decisions_current_benef <- round(decisions_current_benef, decimals) ###


  # Possible next states
  next_states <- states_next[states_next >= (states - E_max + value_inflow) & states_next <= (states + value_inflow + alpha) ]



  # Turbaned energy per transition
  decisions_benef_to_go <- states - next_states + value_inflow
  decisions_benef_to_go <- round(decisions_benef_to_go, decimals) ###
  decisions_benef_to_go <- decisions_benef_to_go[decisions_benef_to_go<=largest_decision]
  decisions_benef_to_go <- decisions_benef_to_go[decisions_benef_to_go<=max(decisions_current_benef)]


  if((largest_decision)>max(decisions_benef_to_go)) decisions_current_benef <- decision_space[1:(length(decisions_current_benef)+1)]

  # List of accessible Rewards
  if(largest_decision>E_max){ provisional_steps <- unique(c(decisions_current_benef, E_max) )
  }else {provisional_steps <- unique(decisions_current_benef)}
  provisional_reward_line <- unique(c(value_reward[seq_along(decisions_current_benef)]))

  decisions <- unique(sort(c(decisions_benef_to_go, decisions_current_benef), decreasing = FALSE))



  #testing rmv NA
  provisional_steps <- provisional_steps[!is.na(provisional_steps)]

  if (length(setdiff(decisions_benef_to_go, decisions_current_benef)) > 0) {

    # boucle sur les quantité de turbinaga possible
    for (index in setdiff(decisions_benef_to_go, decisions_current_benef)) { # index <- 70000 MWh

      # Closest inf simulation constraint
      before <- provisional_steps[index >= provisional_steps - alpha]
      before <- before[length(before)]

      # Closest sup simulation constraint
      after <- provisional_steps[index <= provisional_steps + alpha]
      after <- after[1]

      # For interpolation
      remainder <- (index -  before ) / (after - before)
      # remainder <- round(remainder, decimals) ###
      # remainder <- abs(remainder - trunc(remainder))

      # index_before <- match(before, provisional_steps)
      index_before <- which(num_equal(before, provisional_steps))
      index_before <- round(index_before)
      # index_after <- match(after, provisional_steps)
      index_after <- which(num_equal(after, provisional_steps))
      index_after <- round(index_after)

      # calculate interpolated reward
      interpolation_current_benef <- provisional_reward_line[index_before]*(1-remainder) + provisional_reward_line[index_after]*remainder


      provisional_steps <- unique(sort(c(index, provisional_steps), decreasing = FALSE))

      new_reward_element <- interpolation_current_benef


      # add the reward to the list keeping the increasing order of turbaned energy
      provisional_reward_line <- c(provisional_reward_line[seq_len(index_before)],
                                   new_reward_element, provisional_reward_line[index_after:length(provisional_reward_line)])

    } # fin boucle sur  sur les quantité de turbinaga possible

  } # fin if


  # respect the guide graph con sstraints
  decisions <- decisions[decisions - alpha <= states + value_inflow - level_low]

  decisions <- decisions[decisions + alpha >= states + value_inflow - level_high]

  decisions <- round(decisions, decimals) ###


  # initialize
  tempo <- vector(mode = "numeric", length = length(decisions))
  count_x <- 0



  for (l in decisions) { # l <- 0

    count_x <- count_x + 1


    # Respect Reservoir Capacity
    if ((states - l + value_inflow) >= niveau_max + alpha) {
      next
    }


    states_above <- states_next[states_next > (states - l + value_inflow) - alpha]
    states_below <- states_next[states_next <= (states - l + value_inflow) + alpha]

    next_node_up <- which(num_equal(states_above[length(states_above)], states_next))
    next_node_down <- which(num_equal(states_below[1], states_next))

    remainder <- 0

    if (!num_equal(next_node_up, next_node_down)) {
      remainder <- ((states - l + value_inflow) -states_below[1]) / (states_above[length(states_above)] - states_below[1])
    } else {
      remainder <- 0
    }

    # Bellman value of the next week
    vunw <- next_week_values[next_node_up]
    # if (!is.finite(vunw))
    #    vunw <- 0
    vdnw <- next_week_values[next_node_down]
     # if (!is.finite(vdnw))
     #   vdnw <- 0
    interpolation <- remainder * vunw + (1 - remainder) * vdnw


    tempo[count_x] <- sum(c(provisional_reward_line[num_equal(l, provisional_steps)], interpolation), na.rm = na_rm)
  }


  if (method == "mean-grid") {
    Data_week$value_node[i] <- (max(tempo, na.rm = TRUE))
  } else {
    Data_week$value_node[i] <- (mean_finite(tempo))
  }


#----- little test -----

  if(print_test){
   if (j==51){
   print(sprintf("******Week %d *********",j))
   print(sprintf("State: %d",states))
   print(sprintf("Decisions :  "))
   print(decisions)
   print(sprintf("Rewards : "))
   print(tempo)
   print(sprintf("BELLMAN >>>>> %f",Data_week$value_node[i]))
 }}


  }

#------ grid-mean method---------

# regroup VB by years:
if(method=="grid-mean"){
  Data_week$value_node <- ave(Data_week$value_node, Data_week$years, FUN=function(x)mean(x, na.rm = TRUE))
}

  return(Data_week)
  }
