plot_stratton = function(
    maf,
    sample_name = NULL # any number of samples, if none given all will be plotted
    )
{
    ## Trinucleotide context, middle two nucleotides represent Ref->Alt
    trinuc_context = c("ACAA", "ACAC", "ACAG", "ACAT", "CCAA", "CCAC", "CCAG", "CCAT",
                       "GCAA", "GCAC", "GCAG", "GCAT", "TCAA", "TCAC", "TCAG", "TCAT",
                       "ACGA", "ACGC", "ACGG", "ACGT", "CCGA", "CCGC", "CCGG", "CCGT",
                       "GCGA", "GCGC", "GCGG", "GCGT", "TCGA", "TCGC", "TCGG", "TCGT",
                       "ACTA", "ACTC", "ACTG", "ACTT", "CCTA", "CCTC", "CCTG", "CCTT",
                       "GCTA", "GCTC", "GCTG", "GCTT", "TCTA", "TCTC", "TCTG", "TCTT",
                       "ATAA", "ATAC", "ATAG", "ATAT", "CTAA", "CTAC", "CTAG", "CTAT",
                       "GTAA", "GTAC", "GTAG", "GTAT", "TTAA", "TTAC", "TTAG", "TTAT",
                       "ATCA", "ATCC", "ATCG", "ATCT", "CTCA", "CTCC", "CTCG", "CTCT",
                       "GTCA", "GTCC", "GTCG", "GTCT", "TTCA", "TTCC", "TTCG", "TTCT",
                       "ATGA", "ATGC", "ATGG", "ATGT", "CTGA", "CTGC", "CTGG", "CTGT",
                       "GTGA", "GTGC", "GTGG", "GTGT", "TTGA", "TTGC", "TTGG", "TTGT")

    ### Complementary bases
    nt_comp = list('G'='C', 'A'='T', 'C'='G', 'T'='A')

    ### Load MAF if path provided
    if (is.character(maf)) maf = fread(maf)

    ### Process MAF
    if (is.null(sample_name)) sample_name = unique(maf$Tumor_Sample_Barcode)
    trinuc_maf = filter(maf, Variant_Type == 'SNP', Tumor_Sample_Barcode %in% sample_name) %>%
        mutate(TriNuc_Context = ifelse(
            Reference_Allele %in% c('C','T'),
            paste0(str_sub(Ref_Tri,1,2),Tumor_Seq_Allele2,str_sub(Ref_Tri,3,3)),
            paste0(str_sub(Ref_Tri,1,1),nt_comp[Reference_Allele],nt_comp[Tumor_Seq_Allele2],str_sub(Ref_Tri,3,3)))) %>%
        filter(str_sub(TriNuc_Context,2,2) != str_sub(TriNuc_Context,3,3)) %>%
        group_by(Tumor_Sample_Barcode, TriNuc_Context) %>%
        summarize(TriNuc_Count = n()) %>%
        mutate(TriNuc_Frac = TriNuc_Count/sum(TriNuc_Count)) %>%
        mutate(TriNuc_Context = factor(TriNuc_Context, levels = trinuc_context, ordered = T)) %>%
        mutate(Transition = paste0(str_sub(TriNuc_Context,2,2),'>',str_sub(TriNuc_Context,3,3)))

    ### Plot
    out_plot = ggplot(trinuc_maf, aes(TriNuc_Context, TriNuc_Frac, fill = Transition)) +
        geom_bar(stat = 'identity') +
        theme(text = element_text(family = 'Helvetica', size = 8), axis.text = element_text(color = 'black'),
              axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = .5, hjust = 0, size = 2),
              panel.background = element_blank(), legend.key = element_rect(color = 'white'),
              legend.title = element_blank(), legend.key.size = unit(.25, 'cm'), panel.grid = element_blank(),
              strip.background = element_blank(), strip.text = element_text(size = 10), axis.ticks = element_blank()) +
        labs(x = '', y = 'Fraction') +
        scale_x_discrete(labels = paste0(str_sub(levels(trinuc_maf$TriNuc_Context),1,2), str_sub(levels(trinuc_maf$TriNuc_Context),4,4))) +
        scale_fill_manual(values = c("#1EBFF0", "#050708", "#E62725", "#CBCACB", "#A1CF64", "#EDC8C5")) +
        scale_y_continuous(labels = scales::percent, breaks = seq(0, max(as.numeric(trinuc_maf$TriNuc_Frac)), .05), expand = c(0,0)) +
        geom_hline(yintercept = seq(0, max(as.numeric(trinuc_maf$TriNuc_Frac)), .05), col = 'darkgrey', size = .25, alpha = .5)

    ### Facets if more than one sample
    if (length(sample_name) > 1) out_plot = out_plot + facet_wrap(~Tumor_Sample_Barcode)

    ### Print
    print(out_plot)
}
