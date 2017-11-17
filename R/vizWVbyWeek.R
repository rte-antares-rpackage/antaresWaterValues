#' Visualize Water Weeks for a specified week
#'
#' @param value_node Result from \code{meanGridLayer}.
#' @param week Week to visualize
#' @param add_band Add band around water values, choice between 'none', 'linear', 'spline' or 'both'. 
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
vizWVbyWeek <- function(value_node, week, add_band = 'none', ...) {
  add_band <- match.arg(add_band, c("none", "linear", "spline", "both"))
  args <- list(...)
  value_node <- copy(value_node)
  value_node <- value_node[weeks == week]
  value_node <- value_node[!(!is.na(vu) & is.finite(vu)), vu := NA]
  p <- ggplot2::ggplot(data = value_node)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = states, y = vu))
  if (add_band == "linear") {
    value_node$band <- addBand(
      vu = value_node$vu, 
      states = value_node$states, 
      bandwidth = args$bandwidth %||% 100, 
      failure_cost = args$failure_cost %||% 3000
    )
    p <- p %+% value_node
    p <- p + ggplot2::geom_line(ggplot2::aes(x = states, y = band), color = "firebrick")
  } else if (add_band == "spline") {
    value_node$band <- addBand(
      vu = value_node$vu, 
      states = value_node$states, 
      type = "spline",
      bandwidth = args$bandwidth %||% 100, 
      failure_cost = args$failure_cost %||% 3000
    )
    p <- p %+% value_node
    p <- p + ggplot2::geom_line(ggplot2::aes(x = states, y = band), color = "steelblue")
  } else if (add_band == "both") {
    value_node$band_lin <- addBand(
      vu = value_node$vu, 
      states = value_node$states, 
      type = "linear",
      bandwidth = args$bandwidth %||% 100, 
      failure_cost = args$failure_cost %||% 3000
    )
    value_node$band_spl <- addBand(
      vu = value_node$vu, 
      states = value_node$states, 
      type = "spline",
      bandwidth = args$bandwidth %||% 100, 
      failure_cost = args$failure_cost %||% 3000
    )
    p <- p %+% value_node
    p <- p + ggplot2::geom_line(ggplot2::aes(x = states, y = band_lin), color = "firebrick")
    p <- p + ggplot2::geom_line(ggplot2::aes(x = states, y = band_spl), color = "steelblue")
  }
  p <- p + ggplot2::theme_minimal()
  p
}

