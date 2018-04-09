#' Replace \code{NA}s
#'
#' Abbreviated \code{ifelse} replacement
#'
#' @param x What to replace
#' @param replacement What to replace it with
#'
#' @return Vector where \code{x} is replaced by \code{replacement}

#' @export
if_na = function(x, replacement) {
    ifelse(is.na(x), replacement, x)
}
