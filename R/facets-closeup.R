#' Generate gene-level Facets plot
#'
#' @param path Path to Rdata oject
#' @param gene One or multiple genes to plot
#' @param chrom_range Range of chromosomes
#' @param plot Plot or save as object
#' @param adjust_dlr Set dipLogR to 0, essentially normalizing the log ratio
#' @param subset_snps Subset the SNPs, for easier plotting. Boolean or numeric.
#'
#' @return If \code{plot == T} plots cnlr, valor and icnem objects else returns all as objects
#'
#' @importFrom egg ggarrange
#'
#' @examples
#' \donttest{facets_closeup('path_to_sample.Rdata', 'BRCA1', 17)}

#' @export
facets_closeup = function(path,
                          gene,
                          chrom_range = NULL,
                          plot = T,
                          adjust_dlr = F,
                          subset_snps = F) {

    if (Sys.info()['nodename'] %like% 'selene|luna|juno') {
        basedir = '/home/jonssonp/git/facets-suite/'
    } else {
        basedir = '~/luna/git/facets-suite/'
    }

    ### Source function?
    if (!exists('close.up')) source(paste0(basedir, 'fPlots_ggplot2.R'))

    ### Load Rdata object
    load(path)

    ### Which chromosomes?
    chroms = list('BRCA1' = 16:18,
                  'BRCA2' = 12:14)
    if (is.null(chrom_range)) chrom_range = chroms[[gene]]

    ### Adjust diplogr
    if (adjust_dlr == T) {
        out$jointseg$cnlr = out$jointseg$cnlr - out$dipLogR
        fit$cncf$cnlr.median = fit$cncf$cnlr.median - out$dipLogR
    }

    ### Call close-up function
    gene_cu = close.up(out, fit,
                       gene.name = gene,
                       chrom.range = chrom_range,
                       subset.snps = subset_snps,
                       bed.path = paste0(basedir, 'data/Homo_sapiens.GRCh37.75.canonical_exons.bed'),
                       col.1 = '#6baed6', col.2 = '#969696')

    if (plot) {
        egg::ggarrange(gene_cu$cnlr,
                       gene_cu$valor,
                       gene_cu$icnem,
                       ncol = 1,
                       newpage = F)
    } else {
        gene_cu
    }
}
