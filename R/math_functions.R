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


#----------Bellman Plot--------------

plot_Bellman <- function(value_nodes_dt,week_number,param="vu"){

  temp <- value_nodes_dt[weeks ==week_number]
  temp <- temp[is.finite(vu)&(!is.nan(vu))]

  p1 <- ggplot(data = temp, aes(states , vu)) +geom_line(size=1,color="purple 4")

  setnames(temp,"value_node","Bellman_Value")
  p2 <- ggplot(data = temp, aes(states ,Bellman_Value)) +geom_line(size=1,color="red 4")

  if (param=="vu") {
     print(p1)
  }else if(param=="both") {

    tit <- sprintf("VU and Bellman for Week %d",i)
    title <- ggdraw() + draw_label(tit, fontface='bold')
    p <- plot_grid(p1,p2)
    plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
    }else print(p2)

}

#----------- Bellman monotonicity---------

check_Bellman_inc <- function(results){
print("-----Check Bellman values Monotonicity-----")
for (i in 1:52){
  temp <- results[weeks==i]
  temp <- temp[is.finite(value_node)&(!is.nan(value_node))]
  print(sprintf("week %d --> %s ",i,incr(temp$value_node))
  )}
}

check_vu_dec <- function(results){
  print("-----Check Water Values Monotonicity-----")
  for (i in 1:52){
    temp <- results[weeks==i]
    temp <- temp[is.finite(vu)&(!is.nan(vu))]
    print(sprintf("week %d --> %s ",i,decr(temp$vu))
    )}
}


