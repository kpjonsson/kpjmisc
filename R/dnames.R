#' Custom color palettes
#'
#' Works like \code{duplicated} but returns the actual values.
#'
#' @param name_vector Vector of name.
#'
#' @return The duplicated values in \code{name_vector}.

#' @export
dnames = function(name_vector) {
    name_vector = as.character(name_vector)
    name_vector[which(duplicated(name_vector))]
    }
