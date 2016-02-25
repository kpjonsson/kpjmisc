read_maf = function(maf_path) {
    maf = suppressWarnings(data.table::fread(maf_path, showProgress = F))
    maf_rows = nrow(maf)
    maf_samples = length(unique(maf$Tumor_Sample_Barcode))
    cat(paste0('Read MAF file with ', maf_rows, ' rows and ', maf_samples, ' samples.'))
    maf
}
