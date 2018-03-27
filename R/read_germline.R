#' Read DMP distributed germline file
#'
#' Properly parse fields in DMP's germline distributed files
#'
#' @param path Excel file
#'
#' @return Data frame
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names get_dupes

#' @export
read_germline = function(filepath) {
    out = read_excel(filepath) %>%
        clean_names() %>%
        mutate(patient = str_sub(cvr_sample_id, 1, 9),
               gene = str_extract(gene_exon, regex('[A-Z0-9]+(?=\\()')),
               HGVSc = str_extract(str_replace_all(c_dna_aa_change, ' ', ''), '^[A-Za-z0-9_\\.\\+\\>]+(?=\\()'),
               HGVSp = str_extract(str_replace_all(c_dna_aa_change, ' ', ''), '(?<=\\()[A-Za-z0-9_.\\*]+(?=\\))'),
               mutation = ifelse(HGVSc %in% c('-', NA) &
                                     HGVSp %in% c('-', NA),
                                 str_extract(final_path_score,
                                             'c.[A-Za-z0-9_+>]+|Intragenic Deletion|Intragenic Duplication'),
                                 ifelse(HGVSp %in% c('-', NA),
                                        HGVSc, HGVSp))) %>%
        select(patient, normal = cvr_sample_id, tumor_type, gene, final_path_score,  HGVSc, HGVSp, mutation) %>%
        distinct()

    # Remove empty rows for patients with mutations in any gene
    patient_dupes = get_dupes(out, patient) %>%
        filter(!is.na(gene))

    # marks patients with more than one germline mutation
    filter(out, !(patient %in% patient_dupes$patient & is.na(gene))) %>%
        group_by(patient) %>%
        mutate(multiple_germline = n() > 1) # marks patients with more than one germline mutation
}
