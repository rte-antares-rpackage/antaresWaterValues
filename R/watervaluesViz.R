#' Visualize mean grid layer
#'
#' @param Data Output from \code{Grid_Matrix}.
#' @param filter_penalties Binary paramater. If T, show only watervalues inside rule curves
#'
#' @return a \code{ggplot} object
#' @export
waterValuesViz <- function(Data, filter_penalties=FALSE) {


  value_nodes <- copy(Data)

  if (filter_penalties){
    value_nodes <- dplyr::mutate(value_nodes,
                          vu=dplyr::if_else((.data$states<=.data$level_high)&(.data$states>=.data$level_low),
                                            .data$vu,NaN))
  }

  value_nodes <- value_nodes %>%
    dplyr::mutate(weeks=dplyr::if_else(.data$weeks>=2, .data$weeks-1,52))
  value_nodes <- states_to_percent(value_nodes,states_step_ratio=0.025)
  setnames(value_nodes,"states_round_percent","states")
  p <- ggplot2::ggplot(data = value_nodes)
  p <- p + ggplot2::aes(x = .data$weeks, y = .data$states, fill = .data$vu)
  p <- p + ggplot2::geom_tile()

  p <- p + viridis::scale_fill_viridis(na.value = "transparent")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::labs(x = "Weeks", y = "States")
  p <- p+ggplot2::ggtitle(sprintf("Water Values Plot"))+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(p)
}
