#' Sort oncoprint
#'
#' Sort a gene X sample matrix with binary fields according to oncoprint method
#'
#' @param M Input matrix
#'
#' @return A \code{list} object containing order of genes and samples.

#' @export
sort_oncoprint = function(M) {
    gene_order = sort(rowSums(M, na.rm = T), decreasing = TRUE, index.return = TRUE)$ix
    score_col = function(x) {
        score = 0
        for (i in 1:length(x)) {
            if(x[i]) {
                score = score + 2^(length(x)-i)
            }
        }
        score
    }
    scores = apply(M[gene_order, , drop = FALSE ], 2, score_col)
    sample_order = sort(scores, decreasing = TRUE, index.return = TRUE)$ix
    res = list()
    res$gene_order = gene_order
    res$sample_order = sample_order
    res$M = M[gene_order, sample_0rder]
    res
}
