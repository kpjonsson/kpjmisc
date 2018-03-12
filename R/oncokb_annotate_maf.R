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
