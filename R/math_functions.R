
efficiency_effect <- function(energy,efficiency){

  if(energy<0) energy <- energy*efficiency
  return(energy)

}



build_data_watervalues <- function(watervalues,statesdt,reservoir,
                                   penalty_level_high,penalty_level_low){
  value_nodes_dt <- value_node_gen(watervalues,statesdt,reservoir)

  value_nodes_dt <- value_nodes_dt[value_nodes_dt$weeks!=53,]

  #add penlaties
  value_nodes_dt <- value_nodes_dt %>%
    dplyr::mutate(value_nodes_dt,
           vu_pen=dplyr::case_when(states>level_high ~ vu - penalty_level_high,
                            states<level_low ~ vu + penalty_level_low,
                            TRUE ~ vu)) %>%
    dplyr::rename(vu=.data$vu_pen,vu_pen=.data$vu)

  print(waterValuesViz(value_nodes_dt))
  return(value_nodes_dt)

}


#' Calculate water values from Bellman values
#'
#' @param watervalues an intermediate result in Grid_Matrix contains the bellman values
#' @param statesdt an intermediate result in Grid_Matrix contains the states dicretization
#' @param reservoir an intermediate result in Grid_Matrix contains the reservoir levels
#'
#' @importFrom dplyr left_join
#' @export
value_node_gen <- function(watervalues,statesdt,reservoir){
    value_nodes_dt <- watervalues[, list(value_node = mean_finite(value_node)),
                                  by = list(weeks, statesid)]

    value_nodes_dt[!is.finite(value_node),value_node:=NaN]

    # add states levels
    value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))


    #add reservoir
    names(reservoir)[1] <- "weeks"
    value_nodes_dt <- dplyr::mutate(value_nodes_dt, beg_week=dplyr::if_else(weeks>1,weeks-1,52))
    value_nodes_dt <- dplyr::left_join(value_nodes_dt,reservoir,by=c("beg_week"="weeks")) %>%
      select(-c("beg_week"))


    value_nodes_dt <- value_nodes_dt[order(weeks, -statesid)]
    value_nodes_dt[, value_node_dif := c(NA, diff(value_node)), by = weeks]
    value_nodes_dt[, states_dif := c(NA, diff(states)), by = weeks]
    value_nodes_dt[, vu := (value_node_dif / states_dif )]
    value_nodes_dt[,vu:=round(vu,2)]
    return(value_nodes_dt)
}



#' test a difference vector convergence
#' @param diff_vect is a vector of water values differences
#' @param conv is the value from which the difference become converged
#' @export

converged <- function(diff_vect,conv=1){
  t <- abs(diff_vect)
  t2 <- is.nan(t)
  nan_values <- length(t2[t2==T])
  numeric_values <- length(t)-nan_values

  converged_values <- length(t[t<conv]) - nan_values
  converge_percent <-  converged_values/numeric_values
  return(converge_percent)
  }




#' Check if two numbers are equal.
#' @param x first number to compare.
#' @param y second number to compare.
#' @param tol the minimum difference from which two numbers are equal.
#' @export
num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}


#' Replace outliers values by spline
#' @param vector numeric vector to remove outliers values from it.
#' @importFrom grDevices boxplot.stats
#' @export

correct_outliers <- function(vector) {
  ind_v <- which(is.finite(vector)) # NaN and Inf values shall not be corrected
  v <- vector[ind_v]
  w <- v
  v[v %in% boxplot.stats(v)$out] <- NA
  v <- zoo::na.spline(v, na.rm = FALSE)

  # in case some values cannot be replaced by approximations
  ind_na <- which(is.na(v))
  v[ind_na] <- w[ind_na]

  vector[ind_v] <- v
  return(vector)
}

#----- Mean of finite values
#' Calculate the mean of finite values.
#' Return \code{-Inf} if all \code{-Inf}.
#' @param x numeric vector whose mean is wanted.
#' @export

mean_finite <- function(x) {
  if (all(!is.finite(x))) {
    -Inf
  } else {
    mean(x[is.finite(x)])
  }
}

#---- monotonicity check functions-----
#' Check if the vector has increasing values
#' @param vector numeric vector whose increasing check is wanted.
#' @export

incr <- function(vector){
  all(diff(vector) >= 0)}



#' Check if the vector has decreasing values
#' @param vector numeric vector whose decreasing check is wanted.
#' @export

decr <- function(vector){
  all(diff(vector) <= 0)}




#----------- Bellman monotonicity---------
#' Check the monotonicity of the calculated Bellman values and return a success rate.
#' @param results A data.table contains the Bellman values
#' Obtained using the function \code{Grid_Matrix()}
#' @export

check_Bellman_inc <- function(results){
  print("-----Check Bellman values Monotonicity-----")
  count <- 0
  for (i in 1:52){
    temp <- results[weeks==i]
    temp <- temp[!is.na(temp$value_node)&is.finite(temp$value_node)]
    t <- incr(temp$value_node)
    print(sprintf("week %d --> %s ",i,t))
    if(t) count <- count+1
  }

  print(sprintf("success rate %.0f %% ",(100*count/52)))


  }

#' Check the monotonicity of the calculated  water values and return a success rate.
#' @param results A data.table contains the water values
#' Obtained using the function \code{Grid_Matrix()}
#' @export

check_vu_dec <- function(results){
  print("-----Check Water Values Monotonicity-----")
  count <- 0
  for (i in 1:52){
    temp <- results[weeks==i]
    temp <- temp[is.finite(vu)&(!is.nan(vu))]
    t <- decr(temp$vu)
    print(sprintf("week %d --> %s ",i,t))
    if(t) count <- count+1

    }

  print(sprintf("success rate %.0f %% ",(100*count/52)))
}



#------- states to percent -----
#' Convert Reservoir levels from MWh to percent of reservoir.
#' @param data  A data.table contains the statesid and states columns
#' @param states_step_ratio percent step between two successive levels.
#' @export

states_to_percent <- function(data,states_step_ratio=0.01){


  # rescale levels to round percentages ranging from 0 to 100
  states_ref <- data[, .SD[1], by = statesid, .SDcols = "states"]
  states_ref[, states_percent := 100*states/max(states)]

  interv <- seq(from=0,to=100,by=round(100*states_step_ratio))
  nearest_states <- states_ref$statesid[sapply(interv, function(x) which.min(abs(x - states_ref$states_percent)))]

  states_ref_0_100 <- data.table(
    states_round_percent = interv,
    statesid = nearest_states
  )

  res <- CJ(weeks = unique(data$weeks), states_round_percent = interv)

  res[states_ref_0_100, on = "states_round_percent", statesid := i.statesid]

  res[data, on = c("weeks", "statesid"), value_node := i.value_node]
  res[data, on = c("weeks", "statesid"), value_node_dif := i.value_node_dif]
  res[data, on = c("weeks", "statesid"), vu := i.vu]
  return(res)
  }

#----- check VU decrease in reshaped values ------

#' Check the monotonicity of the calculated the water values after putting them
#' in Antares Format and return a success rate.
#' @param reshaped_results A matrix contains the water values
#' Obtained using the function \code{to_Antares_Format()}
#' @export

check_resh_vu_dec <- function(reshaped_results){
  print("-----Check Water Values Monotonicity-----")
  count <- 0
  for (i in 1:364){
    t <- decr(unlist(reshaped_results[i,]))
    print(sprintf("week %d --> %s ",i,t))
    if(t) count <- count+1

  }

  print(sprintf("success rate %.0f %% ",(100*count/365)))
}


#' Correct concavity of Bellman values to have nice monotony for water values
#'
#' @param weeks Weeks for which we want to correct concavity
#' @param df_value_node DataFrame containing bellman values
#'
#' @return vector of corrected Bellman values
#' @export
correct_concavity <- function(df_value_node, weeks){

  for (s in weeks){
    df_week <- df_value_node[df_value_node$weeks==s,c("weeks","states","value_node","years")]
    df_week <- dplyr::filter(df_week,!is.na(df_week$value_node))
    df_week <- dplyr::filter(df_week,is.finite(df_week$value_node))
    df_week <- unique(df_week)
    df_week <- dplyr::arrange(df_week,states,years)
    n <- nrow(df_week)
    df_week$new_value <- df_week$value_node
    states <- dplyr::distinct(df_week,states) %>% dplyr::pull(states)
    for (x in states){
      df_week <- df_week %>% dplyr::filter(states==x) %>%
        dplyr::rename(value_x=.data$new_value,states_x=states) %>%
        select("years", "states_x", "value_x") %>%
        dplyr::right_join(df_week, by=c("years")) %>%
        dplyr::mutate(coef=(.data$new_value-.data$value_x)/(states-.data$states_x)) %>%
        dplyr::mutate(coef=dplyr::if_else(.data$coef<0,0,.data$coef))
      df_week <- df_week %>% dplyr::filter(states>x) %>% dplyr::group_by(years) %>%
        dplyr::slice(which.max(.data$coef)) %>%
        dplyr::transmute(m=.data$coef,states_y=states, years=years) %>%
        dplyr::right_join(df_week,by="years") %>%
        dplyr::mutate(new_value=dplyr::if_else((states>.data$states_x)&(states<=.data$states_y),
                                 .data$m*(states-.data$states_x)+.data$value_x,.data$new_value)) %>%
        select("years", "weeks", "states", "value_node", "new_value")
    }
    df_value_node[df_value_node$weeks==s,"new_value"]  <- left_join(df_value_node[df_value_node$weeks==s,c("weeks","states","years")],
                                                                    df_week[,c("weeks","states","new_value","years")],
                                                                    by=c("weeks","states","years"))$new_value
  }
  return(df_value_node$new_value)
}
