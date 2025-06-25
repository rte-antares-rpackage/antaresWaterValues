#' Plot water values
#'
#' Plot water values for all weeks and all levels of stock.
#'
#' @param Data Water values result. Output \code{aggregated_results} from \code{Grid_Matrix()}.
#' @param filter_penalties Binary. If \code{TRUE}, show only water values inside rule curves.
#'
#' @returns A \code{ggplot2::ggplot()} object.
#'
#' @export
waterValuesViz <- function(Data, filter_penalties=FALSE) {


  value_nodes <- copy(Data)

  if (filter_penalties){
    value_nodes <- dplyr::mutate(value_nodes,
                          vu=dplyr::if_else((.data$states<=.data$level_high)&(.data$states>=.data$level_low),
                                            .data$vu,NaN))
  }

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
