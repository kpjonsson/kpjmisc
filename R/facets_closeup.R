### Function to do gene-level
facets_closeup = function(path,
                          gene,
                          chrom_range = NULL) {

    ### Source function?
    if (!exists('close.up')) source('/luna/git/facets-suite/fPlots_ggplot2.R')

    ### Load Rdata object
    load(path)

    ### Which chromosomes?
    chroms = list('BRCA1' = 16:18,
                  'BRCA2' = 12:14)
    if (!is.null(chrom_range)) chrom_range = chroms[[gene]]

    ### Call close-up function
    gene_cu = close.up(out, fit,
                       gene.name = gene,
                       chrom.range = chrom_range,
                       subset.snps = F,
                       bed.path = '/luna/git/facets-suite/Homo_sapiens.GRCh37.75.canonical_exons.bed',
                       col.1 = '#6baed6', col.2 = '#969696')

    egg::ggarrange(gene_cu$cnlr,
                   gene_cu$valor,
                   gene_cu$icnem,
                   ncol = 1,
                   newpage = F)
}
