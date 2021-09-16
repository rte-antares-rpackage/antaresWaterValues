#' Visualize mean grid layer
#'
#' @param Data Output from \code{Grid_Matrix}.
#' @param filtre_ratio in [1,0]. Define the percent to keep from water values
#' eliminating the rest (extreme negatives and positives).
#' @param show_negative Boolean. true to show negative water values and
#' False to delete them from the graph.
#' @return a \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_minimal labs
#' @importFrom viridis scale_fill_viridis
#'
# @examples
waterValuesViz <- function(Data, filtre_ratio=1,show_negative=TRUE) {


  value_nodes <- copy(Data)


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
