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
#' @importFrom readr read_tsv
#'
#' @name read_impact_repository_files
NULL

basedir = '/ifs/res/taylorlab/jonssonp/msk-impact/msk-impact'

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
            suppressWarnings(suppressMessages(fread(paste0(basedir, '/', fn))))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
        )

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

    f = gather(f, sample_id, cna, -Hugo_Symbol)

    message(paste('Reading CNA file with:\n',
                  format(length(unique(f$sample_id)), big.mark = ',', scientific = FALSE),
                  'samples'))

    filter(f, cna != 0) # only return actual alterations
}

#' @export
#' @rdname read_impact_repository_files
read_impact_samples = function(filename = NULL) {

    if (!is.null(filename)) {
        fn = filename
    } else {
        fn = 'data_clinical_sample.txt'
    }

    f = tryCatch(
        {
            suppressWarnings(suppressMessages(read_tsv(paste0(basedir, '/', fn), comment = '#')))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )

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
            suppressWarnings(suppressMessages(read_tsv(paste0(basedir, '/', fn), comment = '#')))
        },
        error = function(e) {
            stop('Cannot read file, check that cluster is mounted')
        }
    )

    message(paste('Reading clinical file with:\n',
                  format(length(unique(f$patient_id)), big.mark = ',', scientific = FALSE),
                  'samples'))

    f
}
