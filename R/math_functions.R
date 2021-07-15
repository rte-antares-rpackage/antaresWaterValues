num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}

# expand a vector of 52 weekly water values to a vector of 365 (= 7*52+1) daily
#   values, taking NA's, NaN's and +-Inf's into account
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
  u
}

#----- Mean of finite values
mean_finite <- function(x) {
  if (all(!is.finite(x))) {
    -Inf
  } else {
    mean(x[is.finite(x)])
  }
}




#------ simple interpolation -----
interp <- function(x,x1,y1,x2,y2){

  t <- y2-y1
  tt <- x2-x1
  return(y1+(x-x1)*(t/tt))
}

#---- monotonicity check functions-----
incr <- function(vector1){
  all(diff(vector1) >= 0)}

decr <- function(vector1){
  all(diff(vector1) <= 0)}




#----------- Bellman monotonicity---------

check_Bellman_inc <- function(results){
  print("-----Check Bellman values Monotonicity-----")
  for (i in 1:52){
    temp <- results[weeks==i]
    temp <- temp[!is.na(temp$value_node)&is.finite(temp$value_node)]
    print(sprintf("week %d --> %s ",i,incr(temp$value_node)))
    }
  }

check_vu_dec <- function(results){
  print("-----Check Water Values Monotonicity-----")
  for (i in 1:52){
    temp <- results[weeks==i]
    temp <- temp[is.finite(vu)&(!is.nan(vu))]
    print(sprintf("week %d --> %s ",i,decr(temp$vu)))
    }
}




