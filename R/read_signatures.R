#' Mutational signatures
#'
#' Read output from mutational signatures decomposition
#'
#' @param input_file Path to output from \code{main.py} in \url{github.com/mskcc/mutation-signatures}
#'
#' @return Melted data frame with trivial names of signatures
#'
#' @source \url{cancer.sanger.ac.uk/cosmic/signatures}
#' @name read_signatures
NULL

#' @export
#' @rdname read_signatures
signatures_map = c('Age',
                   'APOBEC',
                   'BRCA',
                   'Smoking',
                   '5',
                   'MMR/MSI',
                   'UV',
                   '8',
                   'POLN',
                   'POLE',
                   'TMZ',
                   '12',
                   'APOBEC',
                   'MMR/MSI',
                   'MMR/MSI',
                   '16',
                   '17',
                   '18',
                   '19',
                   'MMR/MSI',
                   'MMR/MSI',
                   'Aristolochic acid',
                   '23',
                   'Aflatoxin',
                   '25',
                   'MMR/MSI',
                   '27',
                   '28',
                   'Chewing tobacco',
                   '30')

custom_order = c('Age',
                 'APOBEC',
                 'BRCA',
                 'Smoking',
                 'MMR/MSI',
                 'UV',
                 'POLN',
                 'POLE',
                 'TMZ',
                 'Aristolochic acid',
                 'Aflatoxin',
                 'Chewing tobacco',
                 '5',
                 '8',
                 '12',
                 '16',
                 '17',
                 '18',
                 '19',
                 '23',
                 '25',
                 '27',
                 '28',
                 '30')

#' @export
#' @rdname read_signatures
read_signatures = function(input_file) {

    sign = fread(input_file) %>%
        melt(id.vars = c('Sample Name', 'Number of Mutations')) %>%
        dplyr::mutate(variable = str_replace(variable, 'Signature.', ''),
               variable_name = plyr::mapvalues(variable, seq(1,30), signatures_map)) %>%
        dplyr::rename(sample = `Sample Name`, mutation_count = `Number of Mutations`,
               signature = variable, signature_name = variable_name, fraction = value)

    # if (is.null(signature_order)) {
    #     sign = mutate(sign, signature = factor(signature, custom_order, ordered = T))
    #     sign
    # } else {
    #     sign = mutate(sign, signature = factor(signature, signature_order, ordered = T))
    #     sign
    # }

}
