#' Convert to point
#'
#' Convert input to actual points. Useful for setting size of text elements in \code{ggplot2}.
#'
#' @param x Input.
#' @param input_unit Unit of \code{x}, default is millimeters which is default in \code{ggplot2::geom_text}, among other functions.
#'
#' @return Input units converted to points.
#' @source \url{https://twitter.com/andrewheiss/status/1095450321784950785}
#' @importFrom grid convertUnit unit

#' @export
as_points = function(x, input_unit = 'mm') {
    as.numeric(grid::convertUnit(grid::unit(x, 'pt'), input_unit))
}
