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
  v[v < 0] <- 0
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



interp <- function(x,x1,y1,x2,y2){

  t <- y2-y1
  tt <- x2-x1
  return(y1+(x-x1)*(t/tt))
}

incr <- function(vector1){
all(diff(vector1) >= 0)}

decr <- function(vector1){
  all(diff(vector1) <= 0)}

plot_Bellman <- function(value_nodes_dt,week_number,param="vu"){

  state <- value_nodes_dt[weeks ==week_number]$states

  if (param=="vu") {
    VU <-value_nodes_dt[weeks==week_number]$vu
    my_data <- data.frame(state,VU)
    p <- ggplot(data = my_data, aes(state , VU)) +geom_line(size=2)

  }else{
    Bellman <- value_nodes_dt[weeks==week_number]$value_node
    my_data <- data.frame(state,Bellman)
    p <- ggplot(data = my_data, aes(state , Bellman)) +geom_line(size=2)
  }
  # plot (x,y,type='o')

  print(p)

}
