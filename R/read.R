#' Read input file
#'
#' Shorthand for reading files with `vroom::vroom` suppressing messages
#'
#' @param x Input file
#' @param ... Any extra argument passed to `vroom::vroom`
#'
#' @return The result from `vroom::vroom`
#'
#' @importFrom vroom vroom

#' @export
read = function(x, ...) {
  suppressMessages(vroom(x, ...))
}
