#' Annotate OncoKB levels of evidence
#'
#' Adds OncoKB oncogenicity and actionability annotation to list of copy-number alterations. See URLs below.
#'
#' @param cnas Input CNAs
#' @param cancer_types Data frame with samples mapped to cancer type for accurate levels of actionability.
#'
#' @param gene Gene.
#' @param cna_type Type of CNA, mapped from litterals and numeric encoding.
#'
#' @return Annotated CNAs with columns indicating functionality of mutation and levels of actionability.
#'
#' @source \url{oncokb.org}
#' @source \url{github.com/oncokb/oncokb-annotator}
#'
#' @import purrr
#' @importFrom future plan
#' @importFrom furrr future_pmap_dfr
#' @importFrom plyr revalue
#' @importFrom httr modify_url GET content
#' @importFrom stringr str_replace str_extract
#'
#'
#' @name oncokb_annotate_cna
NULL

# Allow parallellization
future::plan(future::multicore)

query_oncokb = function(gene, cna_type, cancer_type = 'CANCER') {

    if (cna_type %in% c('Amplification', 'Deletion')) {

        base_url = 'http://oncokb.org/legacy-api/indicator.json?source=cbioportal'
        oncokb_version = content(GET(base_url))[['dataVersion']]
        tag = paste(gene, cna_type, cancer_type, sep = '-')

        if (!exists('cached_entries')) cached_entries <<- vector(mode = 'list')

        if (!tag %in% names(cached_entries)) {
            query_url = modify_url(base_url, query = list(
                hugoSymbol = gene,
                alteration = cna_type,
                tumorType = cancer_type
            ))

            oncokb_response = GET(query_url)
            oncokb_response = content(oncokb_response)

            cached_entries[[tag]] = oncokb_response
        } else {
            oncokb_response = cached_entries[[tag]]
        }

        drugs = map(oncokb_response$treatments, 'drugs') %>%
            map(., function(x) paste(unlist(x))) %>%
            simplify %>%
            unique

        tibble(oncogenic = as.character(oncokb_response$oncogenic),
               oncokb_level = ifelse(is.null(oncokb_response$highestSensitiveLevel), '',
                                     oncokb_response$highestSensitiveLevel),
               oncokb_resistance_level = ifelse(is.null(oncokb_response$highestResistanceLevel), '',
                                                oncokb_response$highestResistanceLevel),
               oncokb_drugs = ifelse(length(drugs) == 0, '',
                                     paste(unlist(unique(drugs)), collapse = ',')),
               oncokb_version = oncokb_version)
    } else {
        tibble(oncogenic = '')
    }
}

#' @export
#' @rdname oncokb_annotate_cna
oncokb_annotate_cna = function(cnas, cancer_types = NULL)
{
    if (is.null(cancer_types) & !'cancer_type' %in% names(cnas)) {
        message('No cancer types(s) specified, defaults to CANCER')
        cnas$cancer_type = 'CANCER'
    } else if (is.character(cancer_types)) {
        cnas = add_column(cnas, cancer_type = cancer_types)
    } else {
        cnas = left_join(cnas, cancer_types, by = 'sample_id')
    }

    oncokb_cols = mutate(cnas,
                         gene = Hugo_Symbol,
                         cna_type = case_when(
                             is.numeric(cna) & cna < 0 ~ 'Deletion',
                             is.numeric(cna) & cna > 0 ~ 'Amplification',
                             is.character(cna) & tolower(cna) %like% 'del' ~ 'Deletion',
                             is.character(cna) & tolower(cna) %like% 'amp' ~ 'Amplification')
    ) %>%
        select(gene, cna_type, cancer_type) %>%
        future_pmap_dfr(., query_oncokb)

    bind_cols(cnas, oncokb_cols)

}
