suppressPackageStartupMessages({
    library(httr)
})

query_oncokb = function(gene, protein_change, variant_type, start, end, cancer_type = 'CANCER') {

    base_url = 'http://oncokb.org/legacy-api/indicator.json?source=cbioportal'
    tag = paste(gene, protein_change, cancer_type, sep = '-')
    print(tag)
    if (!exists('cached_entries')) cached_entries <<- vector(mode = 'list')

    if (tag %nin% names(tag)) {
        query_url = modify_url(base_url, query = list(
            hugoSymbol = gene,
            alteration = protein_change,
            consequence = variant_type,
            tumorType = cancer_type
        ))

        oncokb_response = GET(query_url)
        oncokb_response = content(oncokb_response)

        cached_entries[[tag]] = oncokb_response
    } else {
        oncokb_response = cached_entries[[tag]]
    }

    drugs = map(oncokb_response$treatments, 'drugs') %>%
        map(., function(x) paste(unlist(x)))

    tibble(oncogenic = as.character(oncokb_response$oncogenic),
           oncokb_level = ifelse(is.null(oncokb_response$highestSensitiveLevel), '',
                                 oncokb_response$highestSensitiveLevel),
           oncokb_resistance_level = ifelse(is.null(oncokb_response$highestResistanceLevel), '',
                                            oncokb_response$highestResistanceLevel),
           oncokb_drugs = ifelse(length(drugs) == 0, '',
                                 paste(unlist(drugs), collapse = ',')))
}
