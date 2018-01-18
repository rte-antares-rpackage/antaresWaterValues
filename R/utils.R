

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
