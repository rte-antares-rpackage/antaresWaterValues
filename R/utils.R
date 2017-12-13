

# Utils ----

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}

