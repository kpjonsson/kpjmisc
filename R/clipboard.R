#' Use OS clipboard
#'
#' Copy/paste objects between R and system clipboard
#'
#' @param df Input object, can be piped through \code{test}
#' @param delim What is the delimiter of pasted object?
#' @param header Does pasted object have a header line?
#'
#' @return None
#'
#' @name clipboard
NULL

#' @export
#' @rdname clipboard
clipboard = function(df) {
   write.table(df, file = pipe('pbcopy'), col.names = T, row.names = F, sep = '\t', quote = F)
}

#' @export
#' @rdname clipboard
pasteboard = function(delim = '\t', header = T) {
    read.csv(pipe('pbpaste'), sep = delim, header)
}
