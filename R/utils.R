

# Utils ----

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}

#' @importFrom grDevices boxplot.stats
remove_outliers <- function(x) {
  x[x %in% boxplot.stats(x)$out] <- NA
  x
}

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}

mean_finite <- function(x, na.rm = FALSE) {
  if (all(!is.finite(x))) {
    -Inf
  } else {
    mean(x[is.finite(x)], na.rm = na.rm)
  }
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

# expand a vector of 52 weekly water values to a vector of 365 (= 7*52+1) daily
#   values, taking NA's, NaN' and +-Inf's into account
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

