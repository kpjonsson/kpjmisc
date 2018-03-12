#' @export
rownum = function(df) {

	if(class(df) %in% c('data.frame', 'data.matrix')) {
		rownames(df) = seq(1, nrow(df))
		return(df)
	}
	else { print('Wrong input type') }

}
