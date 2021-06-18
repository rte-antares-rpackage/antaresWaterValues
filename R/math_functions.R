num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}
