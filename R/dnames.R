# Works like `duplicated` but returns the actual values
#' @export
dnames = function(char_vec) {
    char_vec[which(duplicated(char_vec))]
    }
