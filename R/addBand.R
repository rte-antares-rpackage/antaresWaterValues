
#' Add Band to Water Values
#'
#' @param vu Water values from \code{meanGridLayer}
#' @param states Level of states
#' @param bandwidth Ith of the band to add.
#' @param failure_cost Cost of failure
#'
#' @return A vector of water values with a band
#' @export
#' 
#' @importFrom zoo na.approx
#' @importFrom data.table first last
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
addBand <- function(vu, states, bandwidth = 100, failure_cost = 3000) {
  
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
  n_nas <- length(seq(from = states_below[1], to = states_below[2], by = diff_states))
  val <- data[num_equal(states, states_below[2]), vu_band]
  vals <- zoo::na.approx(c(failure_cost, rep_len(NA, n_nas - 2), val))
  data[states >= states_below[1] & states <= states_below[2], vu_band := vals]
  
  # above
  states_above <- data[!is.na(vu_band), data.table::last(states)] + c(-bandwidth, bandwidth)/100
  states_above[1] <- max(states_above[1], min(data$states))
  states_above[2] <- min(states_above[2], max(data$states))
  n_nas <- length(seq(from = states_above[1], to = states_above[2], by = diff_states))
  val <- data[num_equal(states, states_above[1]), vu_band]
  vals <- zoo::na.approx(c(val, rep_len(NA, n_nas - 2), 0))
  data[states >= states_above[1] & states <= states_above[2], vu_band := vals]
  
  # end
  return(data$vu_band)
}
