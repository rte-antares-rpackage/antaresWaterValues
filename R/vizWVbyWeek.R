#' Visualize Water Weeks for a specified week
#'
#' @param value_node Result from \code{meanGridLayer}.
#' @param week Week to visualize
#' @param add_band Add band around water values
#' @param ... Parametters passed to \code{\link{addBand}}.
#'
#' @return A ggplot2 object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line %+% theme_minimal
#'
#' @examples
#' \dontrun{
#' 
#' # TODO
#' 
#' }
vizWVbyWeek <- function(value_node, week, add_band = FALSE, ...) {
  args <- list(...)
  value_node <- copy(value_node)
  value_node <- value_node[weeks == week]
  value_node <- value_node[!(!is.na(vu) & is.finite(vu)), vu := NA]
  p <- ggplot2::ggplot(data = value_node)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = states, y = vu))
  if (add_band) {
    value_node$band <- addBand(
      vu = value_node$vu, 
      states = value_node$states, 
      bandwidth = args$bandwidth %||% 100, 
      failure_cost = args$failure_cost %||% 3000
    )
    p <- p %+% value_node
    p <- p + ggplot2::geom_line(ggplot2::aes(x = states, y = band))
  }
  p <- p + ggplot2::theme_minimal()
  p
}

