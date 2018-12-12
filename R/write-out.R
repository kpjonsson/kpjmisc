#' Write function
#'
#' Write tab-separate file, wraps \code{write.table}
#'
#' @param x Object to write
#' @param file File to write to
#' @param col.names Column names
#'
#' @return None

#' @export
write_out = function(x, file, col.names = T) {
	write.table(x, file = file, sep = '\t', quote = F, row.names = F, col.names = col.names)
}
