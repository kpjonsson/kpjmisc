#' Genes across versions of MSK-IMPACT
#'
#' @format A data frame with 468 rows and 5 variables:
#' \describe{
#'   \item{Approved.Symbol}{Gene symbol, some deprecated}
#'   \item{Approved.Name}{Full gene name}
#'   \item{Num.Distinct.Exons}{Exons targeted}
#'   \item{First.Design}{First panel version with gene}
#'   \item{Gene.Symbol}{Official gene symbol, use this}
#'   ...
#' }
#' @source \url{http://cmo.mskcc.org/cmo/resources/gene-lists/}
"impact_genes"

# impact_genes = fread('impact-468.txt')
# devtools::use_data(impact_genes)
