#' Use OS clipboard
#'
#' Copy/paste objects between R and system clipboard
#'
#' @param x Input object, can be piped through \code{margrittr} pipe
#' @param delim What is the delimiter of pasted object?
#' @param header Does pasted object have a header line?
#'
#' @return None
#'
#' @name clipboard
NULL

#' @export
#' @rdname clipboard
clipboard = function(x, delim = ',') {

    if (inherits(x, 'data.frame')) {
        write.table(x, file = pipe('pbcopy'), col.names = T, row.names = F, sep = '\t', quote = F)
    } else if (inherits(x, 'character')) {
        x = paste0(x, collapse = delim)
        writeChar(x, con = pipe('pbcopy'))
    }
}

#' @export
#' @rdname clipboard
pasteboard = function(delim = '\t', header = T) {
    read.csv(pipe('pbpaste'), sep = delim, header)
}
