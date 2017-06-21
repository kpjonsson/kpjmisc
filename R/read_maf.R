read_maf = function(maf_path) {
    maf = suppressMessages(suppressWarnings(data.table::fread(maf_path, showProgress = F, verbose = F)))
    maf_rows = nrow(maf)
    maf_samples = length(unique(maf$Tumor_Sample_Barcode))
    cat(paste0('Read MAF file with ', maf_rows, ' rows and ', maf_samples, ' samples.\n'))
    maf %>%
        mutate(t_depth = t_alt_count+t_ref_count, t_var_freq = t_alt_count/t_depth)
}
