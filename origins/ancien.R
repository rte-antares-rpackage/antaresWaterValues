#ancien bellman

# Calcul by week
next_week_values <- week_53

max_hydro <- max(decision_space)


verif_next_week <- list()

{
  if (method == "mean-grid") {
    funGridMean <- mean
  } else {
    funGridMean <- max
  }
} #mean or max method


{
  states1 = watervalues$states
  states_next = watervalues$states_next
  value_reward = reward
  value_inflow = watervalues$hydroStorage
  level_high = watervalues$level_high
  level_low = watervalues$level_low
  value_node_next_week = next_week_values
  E_max = max_hydro
} #calculate_value_node parameters



for (n_cycl in seq_len(nb_cycle)) {

  cat("Calculating value nodes, cycle number:", n_cycl, "\n")

  pb <- txtProgressBar(min = 0, max = 51, style = 3)

  for (i in rev(seq_len(52))) { # rep(52:1, times = nb_cycle)
    watervalues[
      weeks == i,
      value_node := calculate_value_node(
        states = states, states_next = states_next, value_reward = reward,
        value_inflow = hydroStorage, decision_space = decision_space, level_high = level_high,
        level_low = level_low, value_node_next_week = next_week_values, niveau_max = niveau_max, E_max = max_hydro,
        method = method, na_rm = na_rm
      ),
      by = list(years, statesid)
    ]
    if (correct_outliers) {
      watervalues[weeks == i, value_node := correct_outliers(value_node), by = years]
    }

    next_week_values <- watervalues[weeks == i, list(vny = funGridMean(value_node, na.rm = TRUE)), by = statesid][, c(vny)]
    verif_next_week <- c(verif_next_week, list(next_week_values))

    setTxtProgressBar(pb = pb, value = 52 - i)

  }
  close(pb)
}

value_nodes_dt <- watervalues[, list(value_node = funGridMean(value_node, na.rm = TRUE)),
                              by = list(weeks, statesid)]


# add states levels
value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))

# Calculate Usage values
value_nodes_dt <- value_nodes_dt[order(weeks, -statesid)]
value_nodes_dt[, value_node_dif := c(NA, diff(value_node)), by = weeks]
value_nodes_dt[, states_dif := c(NA, diff(states)), by = weeks]
value_nodes_dt[, vu := abs(value_node_dif / states_dif )]

return(value_nodes_dt)

}
