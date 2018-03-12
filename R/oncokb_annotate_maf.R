#' @export
consequence_map = c('3\'Flank'= 'any',
                    '5\'Flank '= 'any',
                    # 'Targeted_Region'= 'inframe_deletion', 'inframe_insertion',
                    'Frame_Shift_Del'= 'frameshift_variant',
                    'Frame_Shift_Ins'= 'frameshift_variant',
                    'In_Frame_Del'= 'inframe_deletion',
                    'In_Frame_Ins'= 'inframe_insertion',
                    'Missense_Mutation'= 'missense_variant',
                    'Nonsense_Mutation'= 'stop_gained',
                    'Nonstop_Mutation'= 'stop_lost',
                    'Splice_Site'= 'splice_region_variant',
                    'Translation_Start_Site'= 'start_lost')

#' @export
query_oncokb = function(gene, protein_change, variant_type, start, end, cancer_type = 'CANCER') {

    base_url = 'http://oncokb.org/legacy-api/indicator.json?source=cbioportal'
    tag = paste(gene, protein_change, cancer_type, sep = '-')

    if (!exists('cached_entries')) cached_entries <<- vector(mode = 'list')

    if (tag %nin% names(tag)) {
        query_url = httr::modify_url(base_url, query = list(
            hugoSymbol = gene,
            alteration = protein_change,
            consequence = variant_type,
            tumorType = cancer_type
        ))

        oncokb_response = httr::GET(query_url)
        oncokb_response = httr::content(oncokb_response)

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

#' @export
oncokb_annotate_maf = function(maf, cancer_types = NULL)
{
    if (is.null(cancer_type) & 'cancer_type' %nin% names(maf)) {
        maf$cancer_type = 'CANCER'
    } else {
        maf = left_join(maf, cancer_types, by = 'Tumor_Sample_Barcode')
    }

    oncokb_cols = mutate(maf,
           gene = Hugo_Symbol,
           protein_change = str_replace(HGVSp_Short, 'p.', ''),
           variant_type = revalue(Variant_Classification, consequence_map, warn_missing = F),
           start = str_extract(Protein_position, '^[0-9]+(?=\\/|\\-)'),
           end = str_extract(Protein_position, '(?<=-)[0-9]+(?=/)'),
           ) %>%
        select(gene, protein_change, variant_type, start, end, cancer_type) %>%
        pmap_dfr(., query_oncokb)

    bind_cols(maf,
              oncokb_cols)

}
