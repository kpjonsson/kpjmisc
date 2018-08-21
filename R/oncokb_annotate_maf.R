#' Annotate read MAF with OncoKB annotation
#'
#' Adds OncoKB oncogenicity and actionatility annotation to VEP-annotated MAF. See URLs below.
#'
#' @param maf Input MAF
#' @param cancer_types Data frame with samples mapped to cancer type for accurate levels of actionability
#'
#' @return Annotated MAF with columns indicating functionality of mutation and levels of actionability
#'
#' @source \url{oncokb.org}
#' @source \url{github.com/oncokb/oncokb-annotator}
#'
#' @import purrr
#' @import furrr
#' @importFrom httr modify_url GET content
#'
#' @name oncokb_annotate_maf
NULL

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

coding_mutations = c('Frame_Shift_Del',
                     'Frame_Shift_Ins',
                     'In_Frame_Del',
                     'In_Frame_Ins',
                     'Missense_Mutation',
                     'Nonsense_Mutation',
                     'Nonstop_Mutation',
                     'Splice_Site',
                     'Targeted_Region',
                     'Translation_Start_Site')

# Allow parallellization
plan(multiprocess)

#' @export
#' @rdname oncokb_annotate_maf
query_oncokb = function(gene, protein_change, variant_type, start, end, cancer_type = 'CANCER') {
  
  if (variant_type != '') {
    
    base_url = 'http://oncokb.org/legacy-api/indicator.json?source=cbioportal'
    oncokb_version = content(GET(base_url))[['dataVersion']]
    tag = paste(gene, protein_change, cancer_type, sep = '-')

    if (!exists('cached_entries')) cached_entries <<- vector(mode = 'list')

    if (!tag %in% names(cached_entries)) {
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
  } else { tibble(oncogenic = '') } 
}

#' @export
#' @rdname oncokb_annotate_maf
oncokb_annotate_maf = function(maf, cancer_types = NULL)
{
    if (is.null(cancer_types) & 'cancer_type' %nin% names(maf)) {
        message('No cancer types(s) specified, defaults to CANCER')
        maf$cancer_type = 'CANCER'
    } else {
        maf = left_join(maf, cancer_types, by = 'Tumor_Sample_Barcode')
    }

    oncokb_cols = mutate(maf,
           gene = Hugo_Symbol,
           protein_change = str_replace(HGVSp_Short, 'p.', ''),
           variant_type = case_when(
               (Variant_Classification %in% coding_mutations & HGVSp_Short != '') | # this is necessary to avoid poorly annotated but likely FP indel calls from Pindel
               (Variant_Classification == 'Splice_Site' & HGVSc != '') |
                Hugo_Symbol == 'TERT' ~
                   revalue(Variant_Classification, consequence_map, warn_missing = F),
             TRUE ~ ''),
           start = str_extract(Protein_position, '^[0-9]+(?=\\/|\\-)'),
           end = str_extract(Protein_position, '(?<=-)[0-9]+(?=/)')
           ) %>%
        select(gene, protein_change, variant_type, start, end, cancer_type) %>%
        pmap_dfr(., query_oncokb)

    bind_cols(maf,
              oncokb_cols)

}
