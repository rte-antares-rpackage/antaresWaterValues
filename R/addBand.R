
#' Add Band to Water Values
#'
#' @param vu Water values from \code{meanGridLayer}
#' @param states Level of states.
#' @param type Perform a linear or a spline interpolation.
#' @param bandwidth Ith of the band to add.
#' @param failure_cost Cost of failure.
#'
#' @return A vector of water values with a band
#' @export
#' 
#' @importFrom zoo na.approx na.spline
#' @importFrom data.table first last
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
addBand <- function(vu, states, type = c("linear", "spline"), bandwidth = 100, failure_cost = 3000) {
  
  type <- match.arg(type)
  num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
    abs(x - y) < tol
  }
  
  data <- data.table(vu = vu, states = states)
  data <- data[, vu_band := vu]
  data <- data[!(!is.na(vu_band) & is.finite(vu_band)), vu_band := NA]
  
  diff_states <- diff(data$states)[1]
  steps <- bandwidth/100/diff_states
  
  # below
  states_below <- data[!is.na(vu_band), data.table::first(states)] + c(-bandwidth, bandwidth)/100
  states_below[1] <- max(states_below[1], min(data$states))
  states_below[2] <- min(states_below[2], max(data$states))
  # n_nas <- length(seq(from = states_below[1], to = states_below[2], by = diff_states))
  # val <- data[num_equal(states, states_below[2]), vu_band]
  # vals <- zoo::na.approx(c(failure_cost, rep_len(NA, n_nas - 2), val))
  # data[states >= states_below[1] & states <= states_below[2], vu_band := vals]
  if (type == "linear") {
    data <- data[states > states_below[1] & states < states_below[2], vu_band := NA]
  }
  data <- data[num_equal(states, states_below[1]), vu_band := failure_cost]
  
  # above
  states_above <- data[!is.na(vu_band), data.table::last(states)] + c(-bandwidth, bandwidth)/100
  states_above[1] <- max(states_above[1], min(data$states))
  states_above[2] <- min(states_above[2], max(data$states))
  # n_nas <- length(seq(from = states_above[1], to = states_above[2], by = diff_states))
  # val <- data[num_equal(states, states_above[1]), vu_band]
  # vals <- zoo::na.approx(c(val, rep_len(NA, n_nas - 2), 0))
  # data[states >= states_above[1] & states <= states_above[2], vu_band := vals]
  if (type == "linear") {
    data <- data[states > states_above[1] & states < states_above[2], vu_band := NA]
  }
  data <- data[num_equal(states, states_above[2]), vu_band := 0]
  
  # Interpolation
  if (type == "linear") {
    data <- data[, vu_band := zoo::na.approx(vu_band, na.rm = FALSE)]
  } else {
    data <- data[, vu_band := exp(zoo::na.spline(log1p(vu_band), na.rm = FALSE, method = "monoH.FC")) - 1]
    data <- data[states > states_above[2], vu_band := NA]
    data <- data[states < states_below[1], vu_band := NA]
  }
  
  # end
  return(data$vu_band)
}
