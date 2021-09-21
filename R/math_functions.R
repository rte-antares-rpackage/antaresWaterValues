
efficiency_effect <- function(energy,efficiency){

  if(energy<0) energy <- energy*efficiency
  return(energy)

}



build_data_watervalues <- function(watervalues,inaccessible_states,statesdt,reservoir){



  value_nodes_dt <- value_node_gen(watervalues,inaccessible_states,statesdt,reservoir)
  value_nodes_dt <- value_nodes_dt[value_nodes_dt$weeks!=53,]
  inacc <- is.finite(value_nodes_dt$value_node)
  temp1 <- value_nodes_dt[weeks==1]$vu
  temp2 <- value_nodes_dt[weeks>1&weeks<53]$vu
  value_nodes_dt[weeks==52]$vu <- temp1
  value_nodes_dt[weeks<52]$vu <- temp2

  value_nodes_dt$inacc <- inacc
  value_nodes_dt[,vu:=vu*inacc]
  print(waterValuesViz(value_nodes_dt,0.99))
  return(value_nodes_dt)

}


#' Calculate water values from Bellman values
#' @param watervalues an intermediate result in Grid_Matrix contains the bellman values
#' @param inaccessible_states Boolean. True to delete unaccessible states of any scenario in the result.
#' @param statesdt an intermediate result in Grid_Matrix contains the states dicretization
#' @param reservoir an intermediate result in Grid_Matrix contains the reservoir levels
#' @importFrom dplyr left_join
#' @export
value_node_gen <- function(watervalues,inaccessible_states=F,statesdt,reservoir){

    # group the years using the mean
    if(inaccessible_states){
      value_nodes_dt <- watervalues[, list(value_node = mean_or_inf(value_node)),
                                    by = list(weeks, statesid)]
    }else{
      value_nodes_dt <- watervalues[, list(value_node = mean_finite(value_node)),
                                    by = list(weeks, statesid)]
    }

    value_nodes_dt[!is.finite(value_node),value_node:=NaN]

    # add states levels
    value_nodes_dt <- merge(x = value_nodes_dt, y = statesdt, by = c("weeks", "statesid"))


    #add reservoir
    names(reservoir)[1] <- "weeks"
    value_nodes_dt <- dplyr::left_join(value_nodes_dt,reservoir,by="weeks")





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

#-----Mean or inf
#' Calculate the mean if there is no infinite or missing value.
#' Return \code{-Inf} in the other case.
#' @param x numeric vector whose mean is wanted.
#' @export
mean_or_inf <- function(x){
  if(any(is.infinite(x)|any(is.nan(x)))){
    return(-Inf)
  }else{
    return(mean(x, na.rm = TRUE))
  }

}


#-----quantile or inf
#' Calculate the quantile if there is no infinite or missing value.
#' Return \code{-Inf} in the other case.
#' @param x numeric vector whose quantile is wanted.
#' @param q_ratio Numeric in [0,1]. Probability of the quantile.
#' @importFrom stats quantile
#' @export
quantile_or_inf <- function(x,q_ratio){
  if(any(is.infinite(x))|any(is.nan(x))){
    return(-Inf)
  }else{
    return(stats::quantile(x,q_ratio))
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

