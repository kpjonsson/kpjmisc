#' Helper functions to read files from MSK-IMPACT repository
#'
#' Helper functions that reads the most commonly used filed from the MSK-IMPACT Mercurial repository.
#'
#' @param filename Override default filenames, assumes they exist in default location
#' @param unfiltered Read MAF containing non-signed out mutation calls
#' @param germline Retain germline calls in MAF
#'
#' @return Data object in tweaked cBioPortal format
#'
#' @importFrom data.table fread
#' @importFrom janitor clean_names
#'
#' @name read_impact_repository_files
NULL

basedir = '/ifs/res/taylorlab/data_repositories/dmp/mskimpact'

#' @export
#' @rdname read_impact_repository_files
read_impact_maf = function(filename = NULL, unfiltered = F, germline = F) {

    if (unfiltered == T) {
        fn = 'data_mutations_unfiltered.txt'
    } else {
        fn = 'data_mutations_extended.txt'
        }
    if (!is.null(filename)) fn = filename

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(fread(paste0(basedir, '/', fn)))) %>%
                mutate(t_var_freq = t_alt_count/(t_alt_count+t_ref_count))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
        )
    f = filter(f, Hugo_Symbol %nin% c('CDKN2Ap16INK4A', 'CDKN2Ap14ARF')) %>%
        mutate(t_var_freq = t_alt_count/(t_alt_count + t_ref_count))

    message(paste('Reading MAF file with:\n',
                  format(nrow(f), big.mark = ',', scientific = FALSE),
                  'lines\n',
                  format(length(unique(f$Tumor_Sample_Barcode)), big.mark = ',', scientific = FALSE),
                  'samples'))

    if (germline == T) { # by default remove germline calls
        f
    } else {
        filter(f, Mutation_Status != 'GERMLINE')
    }
}

#' @export
#' @rdname read_impact_repository_files
read_impact_cna = function(filename = NULL) {

    if (!is.null(filename)) {
        fn = filename
    } else {
        fn = 'data_CNA.txt'
    }

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(fread(paste0(basedir, '/', fn))))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )

    f = gather(f, sample_id, cna, -Hugo_Symbol) %>%
        mutate(Hugo_Symbol = ifelse(Hugo_Symbol %like% '(^CDKN2A|^CDKN2B)', 'CDKN2A/B', Hugo_Symbol)) %>%
        distinct(sample_id, Hugo_Symbol, .keep_all = T)

    message(paste('Reading CNA file with:\n',
                  format(length(unique(f$sample_id)), big.mark = ',', scientific = FALSE),
                  'samples'))

    filter(f, cna != 0) # only return actual alterations
}

#' @export
#' @rdname read_impact_repository_files
read_impact_fusions = function(filename = NULL) {

    if (!is.null(filename)) {
        fn = filename
    } else {
        fn = 'data_fusions.txt'
    }

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(fread(paste0(basedir, '/', fn))))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )

    f = select(f, Tumor_Sample_Barcode, Hugo_Symbol, everything()) %>%
        rename_at(vars(-matches('Tumor_Sample_Barcode|Hugo_Symbol')), funs(str_to_lower(.))) %>%
        select(-center, -method, -entrez_gene_id)

    message(paste('Reading fusion file with:\n',
                  format(length(unique(f$Tumor_Sample_Barcode)), big.mark = ',', scientific = FALSE),
                  'samples'))

    f
}

#' @export
#' @rdname read_impact_repository_files
read_impact_samples = function(filename = NULL) {

    if (!is.null(filename)) {
        fn = filename
    } else {
        fn = paste0(basedir, '/', 'data_clinical_sample.txt')
    }

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(fread(fn, skip = 'SAMPLE_ID')))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )
    f = clean_names(f)
    message(paste('Reading clinical file with:\n',
                  format(length(unique(f$sample_id)), big.mark = ',', scientific = FALSE),
                  'samples'))

    f
}

#' @export
#' @rdname read_impact_repository_files
read_impact_patients = function(filename = NULL) {

    if (!is.null(filename)) {
        fn = filename
    } else {
        fn = 'data_clinical_patient.txt'
    }

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(fread(paste0(basedir, '/', fn), skip = 'PATIENT_ID')))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )
    f = clean_names(f)
    message(paste('Reading clinical file with:\n',
                  format(length(unique(f$patient_id)), big.mark = ',', scientific = FALSE),
                  'samples'))

    f
}
