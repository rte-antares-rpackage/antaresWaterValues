#' Visualize mean grid layer
#'
#' @param value_nodes Output from \code{watervalues} or \code{Grid_Matrix}.
#' @param add_band Add band around water values, \code{TRUE} or \code{FALSE}.
#' @param type Perform a linear or a spline interpolation.
#' @param bandwidth Ith of the band to add.
#' @param failure_cost Cost of failure.
#'
#' @return a \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_minimal labs
#' @importFrom viridis scale_fill_viridis
#'
# @examples
waterValuesViz <- function(Data, filtre_ratio=1,show_negative=TRUE, add_band = FALSE,
                           type = c("linear", "spline"),
                           bandwidth = 100, failure_cost = 3000) {


  value_nodes <- copy(Data)

  if (add_band) {
    value_nodes <- copy(value_nodes)
    value_nodes <- value_nodes[, vu_band := addBand(
      vu = vu, states = states,
      failure_cost = failure_cost, type = type,
      bandwidth = bandwidth
    ), by = weeks]
  }
  q <- quantile(value_nodes$vu,filtre_ratio,na.rm = TRUE)
  value_nodes[vu>q,vu:=q]
  q_down <- quantile(value_nodes$vu,1-filtre_ratio,na.rm = TRUE)
  value_nodes[vu<q_down,vu:=q_down]
  if(!show_negative){  value_nodes[vu<0,vu:=0]
}
  value_nodes <- states_to_percent(value_nodes,states_step_ratio=0.025)
  setnames(value_nodes,"states_round_percent","states")
  p <- ggplot2::ggplot(data = value_nodes)
  if (add_band) {
    p <- p + ggplot2::aes(x = weeks, y = states, fill = vu_band)
  } else {
    p <- p + ggplot2::aes(x = weeks, y = states, fill = vu)
  }
  p <- p + ggplot2::geom_tile()
  p <- p + viridis::scale_fill_viridis(na.value = "transparent")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::labs(x = "Weeks", y = "States")
  p <- p+ggtitle(sprintf("Water Values Plot"))+theme(plot.title = element_text(hjust = 0.5))

  return(p)
}
