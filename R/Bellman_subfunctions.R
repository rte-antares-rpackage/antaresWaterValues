
#' An intermediate function in bellman values calculation.
#' Select the next week bellman values of a position from all bellman values.
#' @param  next_week_values_l list of all bellman values of the next week.
#' @param max_mcyear the maximum number of scenarios.
#' @param i the number of row in the table of calculation.
#' @export

next_week_value <- function(next_week_values_l,max_mcyear,i){

  year_rows <- length(next_week_values_l)/max_mcyear

  r <- (i)%/%year_rows
  if(i%%year_rows==0) r <- r-1
  next_week_values <- next_week_values_l[((r*year_rows)+1):(((r+1)*year_rows))]

  return(next_week_values)


}


# max possible decision --------


largest_decisions <- function(states,value_inflow,niveau_max,E_max,P_max)

   {
    largest_turb <- min(c(states + value_inflow, E_max,niveau_max), na.rm = TRUE)

    largest_pump <- -min(c(niveau_max-(states + value_inflow), P_max), na.rm = TRUE)

    largest_decisions <- list()
    largest_decisions$largest_turb <- largest_turb
    largest_decisions$largest_pump <- largest_pump
    class(largest_decisions) <- "largest possible decisions"
    return(largest_decisions)
    }



# the decisions that respect the max possible decision used in simulation constraints
check_largest_decisions <- function(decision_space,largest_decisions,alpha){


  decisions_current_benef <- decision_space[decision_space <= largest_decisions$largest_turb + alpha]
  decisions_current_benef <- decision_space[decision_space >= largest_decisions$largest_pump - alpha]
  decisions_current_benef <- round(decisions_current_benef) ###

  return(decisions_current_benef)
  }

#the turbaned energy by transition that respects the constraints

turbined_energy <- function(states,next_states,value_inflow,decisions_current,largest_decisions){


  turbined_energy <- states - next_states + value_inflow
  turbined_energy <- round(turbined_energy) ###

  turbined_energy <- turbined_energy[turbined_energy<=largest_decisions$largest_turb]
  turbined_energy <- turbined_energy[turbined_energy<=max(decisions_current)]

  turbined_energy <- turbined_energy[turbined_energy>=largest_decisions$largest_pump]
  turbined_energy <- turbined_energy[turbined_energy>=min(decisions_current)]

  return(turbined_energy)
}




# Generate decisions that cover possible turbined energy
decisions_cover <- function(turbined_energy,decisions_current){
  decisions_cover <- turbined_energy
  if(is.na(match(max(turbined_energy),decisions_current)))
  {
    turbs_dec <- decisions_current[decisions_current<max(turbined_energy)]
    decisions_cover <- append(decisions_cover,
                              decisions_current[(length(turbs_dec)+1)],after = length(decisions_cover))
  }

  if(is.na(match(min(turbined_energy),decisions_current))){
    pumps_dec <- decisions_current[decisions_current<=min(turbined_energy) ]
    decisions_cover <- append(decisions_cover,
                              pumps_dec[(length(pumps_dec))],after =0)
  }

  return(decisions_cover)
}


# List of accessible Rewards
accessible_rewards <- function(decision_cover,decision_space,value_reward){
  provisional_steps <- decision_space[decision_space<=max(decision_cover)&decision_space>=min(decision_cover)]

  i <- which(decision_space==min(provisional_steps))
  j <- which(decision_space==max(provisional_steps))


  provisional_reward_line <- unique(value_reward[i:j])


  provisional <- list()
  provisional$steps <- provisional_steps
  provisional$rewards <- provisional_reward_line
  class(provisional) <- "Transitions and their rewards"
  return(provisional)
}


#generate possible decisions

generate_decisions <- function(turbined_energy,decisions_cover,E_max,P_max){

  decisions <- c(turbined_energy, decisions_cover)
  if(min(decisions)<P_max) decisions <- append(decisions,P_max)
  if(max(decisions)>E_max) decisions <- append(decisions,E_max)
  decisions <- unique(sort(decisions, decreasing = FALSE))
  return(decisions)
}



generate_decisions_rewards <- function(decisions,step_reward,alpha)
  {

  provisional_steps <- step_reward$steps
  provisional_reward_line <- step_reward$rewards


  if (length(setdiff(decisions, provisional_steps)) > 0) {

    # boucle sur les quantité de turbinaga possible
    for (index in setdiff(decisions, provisional_steps)) { # index <- 70000 MWh

      # Closest inf simulation constraint
      before <- provisional_steps[index >= provisional_steps - alpha]

      before <- before[length(before)]

      # Closest sup simulation constraint
      after <- provisional_steps[index <= provisional_steps + alpha]
      after <- after[1]

      # For interpolation
      remainder <- (index -  before ) / (after - before)

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



  step_reward$steps <- provisional_steps
  step_reward$rewards <- provisional_reward_line

  return(step_reward)
}




# eliminate decisions that violate the guide graph constraints


guide_cs_check <-function(decisions,states,value_inflow,level_high,level_low,alpha){


  decisions <- decisions[decisions - alpha <= states + value_inflow - level_low]

  decisions <- decisions[decisions + alpha >= states + value_inflow - level_high]

  return(decisions)
}



# calculate bellman values

bellman_calculator <- function(decisions,next_week_values,decision_rewards,states,value_inflow,niveau_max,states_next,alpha,na_rm){

    # initialize
    Bellman_values <- vector(mode = "numeric", length = length(decisions))
    count_x <- 0
    provisional_steps <- decision_rewards$steps
    provisional_reward_line <- decision_rewards$rewards

    for (l in decisions) {

      count_x <- count_x + 1


      # Respect Reservoir Capacity
      if ((states - l + value_inflow) >= niveau_max + alpha) {
        Bellman_values[count_x] <- -1
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

      vdnw <- next_week_values[next_node_down]

      interpolation <- remainder * vunw + (1 - remainder) * vdnw


      Bellman_values[count_x] <- sum(c(provisional_reward_line[num_equal(l, provisional_steps)], interpolation), na.rm = na_rm)
    }

    return(Bellman_values)

  }
