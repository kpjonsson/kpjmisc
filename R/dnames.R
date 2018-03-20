#' Custom color palettes
#'
#' Works like \code{duplicated} but returns the actual values
#'
#' @return The duplicated values

#' @export
dnames = function(char_vec) {
    char_vec[which(duplicated(char_vec))]
    }
