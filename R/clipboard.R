#' @export
clipboard = function(df) {
   write.table(df, file = pipe('pbcopy'), col.names = T, row.names = F, sep = '\t', quote = F)
}
