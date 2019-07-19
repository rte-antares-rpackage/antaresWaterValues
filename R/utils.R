

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
