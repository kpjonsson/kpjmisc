#' Copy object to clipboard
#'
#' @param df input object, can be piped through \code{test}
#'
#' @return None
#'
#' @export
clipboard = function(df) {
   write.table(df, file = pipe('pbcopy'), col.names = T, row.names = F, sep = '\t', quote = F)
}
