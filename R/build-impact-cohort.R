#' Title
#'
#' @param ids
#' @param name
#' @param directory
#'
#' @return
#' @export
#'
#' @importFrom dplyr case_when pull
#' @importFrom purrr keep
#' @importFrom annotateMaf oncokb_annotate_maf
#'
#' @examples

#' @export
# build_impact_cohort = function(ids, cohort_name = NULL, directory = getwd()) {
#     ids = keep(ids %like% 'P-[0-9]{7}') %>%
#         unique()
#
#     if (is.null(ids) | length(ids) == 0) {
#         stop('No samples valid IDs provided.', call. = FALSE)
#     }
#
#     if (is.null(cohort_name)) {
#         cohort_name = ''
#     } else {
#         paste0(cohort_name, '_')
#     }
#
#     if (nchar(ids[1]) == 9) {
#         samples = read_impact_samples() %>%
#             filter(patient_id %in% ids) %>%
#             pull(sample_id)
#     } else {
#         samples = ids
#     }
#
#     message(paste('Preparing data from', length(unique(samples), 'unique samples')))
#
#     # Gather, write out data
#     maf = read_impact_maf(unfiltered = TRUE, germline = TRUE)
#     maf_germline = filter(maf, Mutation_Status == 'GERMLINE')
#     maf_somatic = filter(maf, Mutation_Status != 'GERMLINE',
#                          ((Tumor_Sample_Barcode %like% 'IM3' &
#                               Hugo_Symbol %in% impact_genes$Gene.Symbol[impact_genes$First.Design == 'IMPACT-341']) |
#                          (Tumor_Sample_Barcode %like% 'IM5' &
#                               Hugo_Symbol %in% impact_genes$Gene.Symbol[impact_genes$First.Design != 'IMPACT-468']) |
#                          (Tumor_Sample_Barcode %like% 'IM3' &
#                               Hugo_Symbol %in% impact_genes$Gene.Symbol) |
#                          (Hugo_Symbol == 'TERT' & Variant_Classification == "5'Flank")
#                          )) %>%
#         annotateMaf::oncokb_annotate_maf()
#
#     write_out(maf, paste0(directory, '/', cohort_name, 'data_mutations_unfiltered.txt'))
#     write_out(maf_germline, paste0(directory, '/', cohort_name, 'data_mutations_germline.txt'))
#     write_out(maf_somatic, paste0(directory, '/', cohort_name, 'data_mutations_extended.txt'))
#
#     cnas = read_impact_cna() %>%
#         filter(sample_id %in% samples) %>%
#
# }
#
#
# basedir = '/ifs/res/taylorlab/jonssonp/ntrk_rosene/data'
# sample_list = fread('/ifs/res/taylorlab/jonssonp/ntrk_rosene/2019-02-22_sample-list.txt') %>%
#     clean_names()
#
#
# # Get FACETS data, make symlinks
# facets = get_facets(samples)
#
# system(paste('mkdir', paste0(basedir, '/facets')))
# walk(facets$run_prefix, function(x) {
#     sample_name = str_extract(x, 'P-[0-9]{7}-T[0-9]{2}-IM[0-9]{1}_P-[0-9]{7}-N[0-9]{2}-IM[0-9]{1}')
#     system(paste('mkdir', paste0(basedir, '/facets/', sample_name)))
#     system(paste('lndir', paste0(x), paste0(basedir, '/facets/', sample_name)),
#            ignore.stderr = TRUE, ignore.stdout = TRUE) # produces verbose, confusing messages
# })
#
#
#
# # Retrieve list of FACETS samples with path to output -------------------------------------------------------------
# get_facets = function(samples) {
#
#     # Load manifest, check for newest
#     manifest = paste0('/ifs/res/taylorlab/impact_facets/facets_0.5.14/manifests/impact_facets_manifest_',
#                       gsub('-', '_', seq(lubridate::today(), lubridate::today()-5, by = '-1 day')),
#                       '.txt') %>%
#         detect(., file.exists)
#
#     if (is.null(manifest)) {
#         stop('No FACETS manifest from the last 5 days found.', call. = F)
#     } else {
#         manifest = fread(manifest)
#     }
#
#     # Subset
#     filter(manifest, tumor_sample %in% samples)
# }
#
#
# # Retrieve MAF ----------------------------------------------------------------------------------------------------
# get_mutations = function() {
#
# }
#
# # Retrieve CNAs ---------------------------------------------------------------------------------------------------
# get_cnas = function() {
#
# }
#
# get_svs = function() {
#
# }
#
# create_postprocess_script = function() {
#
# }
