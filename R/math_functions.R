#' @export

num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}

# expand a vector of 52 weekly water values to a vector of 365 (= 7*52+1) daily
#   values, taking NA's, NaN's and +-Inf's into account
#' @export

expand_to_days <- function(v) {
  v[!is.finite(v)] <- NaN
  v <- sapply(v, function(x) c(rep(if (is.finite(x)) NA else NaN, 6), x))
  v <- c(v, if (is.finite(v[length(v)])) NA else NaN) # 365th day of the year
  u <- v[!is.nan(v)]
  if (length(u) > 0) {
    v[!is.nan(v)] <- na.spline(u)
  }
  v[is.nan(v)] <- 0
  v
}

#' @importFrom grDevices boxplot.stats
#' @export

correct_outliers <- function(u) {
  ind_v <- which(is.finite(u)) # NaN and Inf values shall not be corrected
  v <- u[ind_v]
  w <- v
  v[v %in% boxplot.stats(v)$out] <- NA
  v <- zoo::na.spline(v, na.rm = FALSE)

  # in case some values cannot be replaced by approximations
  ind_na <- which(is.na(v))
  v[ind_na] <- w[ind_na]

  u[ind_v] <- v
  return(u)
}

#----- Mean of finite values
#' @export

mean_finite <- function(x) {
  if (all(!is.finite(x))) {
    -Inf
  } else {
    mean(x[is.finite(x)])
  }
}

#-----Mean or inf
#' @export
mean_or_inf <- function(x){
  if(any(is.infinite(x)|any(is.nan(x)))){
    return(-Inf)
  }else{
    return(mean(x, na.rm = TRUE))
  }

}


#-----quantile or inf
#' @importFrom stats quantile
#' @export
quantile_or_inf <- function(x,q_ratio){
  if(any(is.infinite(x))|any(is.nan(x))){
    return(-Inf)
  }else{
    return(stats::quantile(x,q_ratio))
  }

}


#------ simple interpolation -----
#' @export

interp <- function(x,x1,y1,x2,y2){

  t <- y2-y1
  tt <- x2-x1
  return(y1+(x-x1)*(t/tt))
}

#---- monotonicity check functions-----
#' @export

incr <- function(vector1){
  all(diff(vector1) >= 0)}

#' @export

decr <- function(vector1){
  all(diff(vector1) <= 0)}




#----------- Bellman monotonicity---------
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

