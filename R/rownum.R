#' Row numbers
#'
#' Add row numbers as row names
#'
#' @param df Data frame
#'
#' @return Data frame with row numbers as row names
#'
#' @export
rownum = function(df) {

	if(class(df) %in% c('data.frame', 'data.matrix')) {
		rownames(df) = seq(1, nrow(df))
		return(df)
	}
	else { print('Wrong input type') }

}
