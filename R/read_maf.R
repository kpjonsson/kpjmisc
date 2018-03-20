#' Read MAF file
#'
#' Simple function to read MAF file and a \code{t_var_freq} column
#'
#' @param maf_path Path to input file
#'
#' @return None


#' @export
read_maf = function(maf_path) {
    maf = suppressMessages(suppressWarnings(data.table::fread(maf_path, showProgress = F, verbose = F)))
    maf_rows = nrow(maf)
    maf_samples = length(unique(maf$Tumor_Sample_Barcode))
    cat(paste0('Read MAF file with ', maf_rows, ' rows and ', maf_samples, ' samples.\n'))
    if (!'t_var_freq' %in% names(maf)) {
        maf = mutate(maf,
                     t_depth = as.numeric(t_alt_count)+as.numeric(t_ref_count),
                     t_var_freq = as.nummeric(t_alt_count)/t_depth)
    }
    maf
}
