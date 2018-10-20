#' Annotate read MAF with hotspots
#'
#' Adds hotspot annotation to VEP-annotated MAF. Sources of default hotspots below.
#'
#' @param maf Input MAF
#' @param hotspot Custom list of hotspots
#'
#' @return Annotated MAF with columns \code{snv_hotspot}, \code{threeD_hotspot}, \code{indel_hotspot_type} and \code{Hotspot} indicating types of hotspots
#'
#' @source \url{cancerhotspots.org}
#' @source \url{3dhotspots.org}
#' @source \url{www.ncbi.nlm.nih.gov/pubmed/26619011}
#'
#' @importFrom tidyr replace_na
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_lines
#'
#' @name hotspot_annotate_maf
NULL

# Source of hotspots
hs_from_local = c('/luna/work/hotspots/24k/hotspots_24k_FULL.txt',
                  '/luna/work/hotspots/nbt/hotspots_NBT_FULL_FP_annotated.txt',
                  '/luna/work/hotspots/3d/hotspots_FP_annotated.txt')

hs_from_cluster = c('/ifs/work/taylorlab/jonssonp/hotspots/24k/hotspots_24k_FULL.txt',
                    '/ifs/work/taylorlab/jonssonp/hotspots/nbt/hotspots_NBT_FULL_FP_annotated.txt',
                    '/ifs/work/taylorlab/jonssonp/hotspots/3d/hotspots_FP_annotated.txt')

if (Sys.info()[['nodename']] == 'lski2423') {
    hotspot_files =  hs_from_local
    } else {
    hotspot_files = hs_from_cluster
    }

load_gene_annotation = function() {
    read_lines('http://oncokb.org/api/v1/genes') %>%
        fromJSON()
}

#' @export
#' @rdname hotspot_annotate_maf
hotspot_annotate_maf = function(maf, hotspots = NULL)
{
    if (!inherits(maf, 'data.frame')) stop('Input MAF must be a data frame, preferrable VEP annotated')
    if (!is.null(hotspots) & !inherits(hotspots, 'data.frame')) {
        stop('Hotspots must be a data frame with a least columns Gene and Residue')
    }

    if (any(grepl('hotspot', tolower(names(hotspots))))) message('Hotspot columns in MAF might be overwritten, check names')

    # Read default hotspot lists if user does not supply
    gene_annotation = load_gene_annotation()

    if (is.null(hotspots)) {

        if (!all(map_lgl(hotspot_files, file.exists))) {
            stop(paste0('Cannot read hotspot files from:\n', paste0(hotspot_files, collapse = '\n')),
                        call. = F)
        }

        hotspots = map_dfr(hotspot_files, function(x) fread(x) %>% mutate(source = x)) %>%
            mutate(indel_hotspot = Type == 'in-frame indel',
                   indel_hotspot = ifelse(is.na(indel_hotspot), FALSE, indel_hotspot),
                   threeD_hotspot = ifelse(Class %in% c('Hotspot-linked', 'Cluster-exclusive'), TRUE, FALSE),
                   snv_hotspot = source %like% 'NBT|24k' & indel_hotspot == FALSE,
                   Pos = ifelse(indel_hotspot == F, str_extract(Residue, '(?<=[A-Z])[0-9]+'), NA),
                   Start = ifelse(indel_hotspot == T, str_extract(Residue, '^[0-9]+(?=-)'), NA),
                   End = ifelse(indel_hotspot == T, str_extract(Residue, '(?<=-)[0-9]+$'), NA),
                   Pos = ifelse(indel_hotspot == T & is.na(Start), Residue, Pos),
                   previous_mutations = str_replace_all(Variants, ':[0-9]+\\|?', ',')) %>%
            replace_na(list(false_positive = FALSE)) %>%
            group_by(Gene, Residue, Pos, Start, End) %>%
            dplyr::summarize(indel_hotspot = any(indel_hotspot == T, na.rm = T),
                      snv_hotspot = any(snv_hotspot == T, na.rm = T),
                      threeD_hotspot = any(threeD_hotspot == T, na.rm = T),
                      previous_mutations = paste(c(unique(unlist(strsplit(previous_mutations, ',')))), collapse = ','),
                      false_positive = all(false_positive == T, na.rm = T)) %>%
            ungroup() %>%
            mutate(tag = str_c(Gene, Pos),
                   Pos = as.numeric(Pos),
                   Start = as.numeric(Start),
                   End = as.numeric(End)) %>%
            filter(false_positive == FALSE)
    }

    # Function that deals with indel hotspots
    tag_indel_hotspot = function(gene, hgvsp_short, start, end, indel_length) {
        is_hotspot = 'none'
        gene_hotspots = filter(hotspots, Gene == gene, indel_hotspot == T) %>% # checks if previously identified
            pull(previous_mutations) %>%
            str_split(',') %>%
            unlist() %>%
            discard(. == '')
        start_res = as.numeric(str_extract(gene_hotspots, '[0-9]+(?=_)'))
        end_res = as.numeric(str_extract(gene_hotspots, '(?<=_[A-Z]{1})[0-9]+'))
        longest_variant = max(end_res-start_res, na.rm = T)
        indel_hs = filter(hotspots, Gene == gene, indel_hotspot == T) %>% # checks if overlap with hotspot intervals
            filter(Start-1 <= (start+2) & End >= (end-2) & indel_length <= (longest_variant+1)) # allow for wiggle room in both directions but not too long indel
        if (nrow(indel_hs) > 0 | str_replace(hgvsp_short, 'p.', '') %in% gene_hotspots) {
            is_hotspot = 'prior'
        } else if (end - start <= 5) { # if short hotspot overlapping known hotspot
            snv_hs = filter(hotspots, Gene == gene & (indel_hotspot == F | is.na(End))) %>% # includes indel hotspots that only cover one codon
                filter(between(Pos, start, end))
            if (nrow(snv_hs) > 0) is_hotspot = 'novel'
        }
        return(is_hotspot)
    }

    tag_onp_hotspot = function(gene, type, trunc, start, end, mut_length) {
        is_hotspot = FALSE
        gene_hotspots = filter(hotspots, Gene == gene, snv_hotspot == T) %>% # checks if previously identified
            select(tag, previous_mutations) %>%
            mutate(previous_mutations = map(previous_mutations, ~unlist(str_split(., ','))))
        if(type == 'SNP' & trunc == F) {
            is_hotspot = str_c(gene, start) %in% gene_hotspots$tag
        } else if (type == 'SNP' & trunc == T) {
            is_hotspot = str_c(gene, start) %in% gene_hotspots$tag &
                gene %in% gene_annotation$hugoSymbol[gene_annotation$tsg == T]
        } else if (mut_length > 3) {
            is_hotspot = FALSE
        } else if (any(c(str_c(gene, start), str_c(gene, end)) %in% gene_hotspots$tag) &
                   trunc == F) {
            is_hotspot = TRUE
        } else if (any(c(str_c(gene, start), str_c(gene, end)) %in% gene_hotspots$tag) &
                   trunc == T) {
            is_hotspot = gene %in% gene_annotation$hugoSymbol[gene_annotation$tsg == T]
        }
        return(is_hotspot)
    }

    maf = mutate(maf,
                 residue = str_extract(Protein_position, '^[0-9]+(?=/|-)'),
                 start_residue = residue,
                 end_residue = str_extract(Protein_position, '(?<=-)[0-9]+(?=/)'),
                 end_residue = ifelse(is.na(end_residue) & Variant_Classification %like% 'In_Frame',
                                      start_residue, end_residue)) %>%
        replace_na(list(start_residue = 0, end_residue = 0)) %>%
        rowwise() %>%
        mutate(
            start_residue = as.numeric(start_residue),
            end_residue = as.numeric(end_residue),
            variant_length = case_when(
                Variant_Type %in% c('SNP', 'DNP', 'TNP', 'ONP') ~ nchar(Reference_Allele)/3,
                Variant_Classification == 'In_Frame_Del' ~ nchar(Reference_Allele)/3,
                Variant_Classification == 'In_Frame_Ins' ~ nchar(Tumor_Seq_Allele2)/3,
            ),
            snv_hotspot = ifelse(Variant_Type %like% 'NP$' &
                                     Variant_Classification %in% coding_mutations &
                                     !is.na(residue),
                                 tag_onp_hotspot(
                                     Hugo_Symbol,
                                     Variant_Type,
                                     Variant_Classification %in% truncating_mutations,
                                     as.numeric(start_residue),
                                     as.numeric(end_residue),
                                     variant_length
                                     ),
                                 FALSE),
            threeD_hotspot = ifelse(Variant_Type %in% c('SNP', 'DNP') &
                                         Variant_Classification %in% coding_mutations & !is.na(residue),
                                    str_c(Hugo_Symbol, residue) %in% hotspots$tag[hotspots$threeD_hotspot == T],
                                    FALSE),
            indel_hotspot_type = ifelse(Variant_Classification %like% 'In_Frame',
                                        tag_indel_hotspot(Hugo_Symbol,
                                                          HGVSp_Short,
                                                          as.numeric(start_residue),
                                                          as.numeric(end_residue),
                                                          variant_length),
                                        'none'),
            indel_hotspot = indel_hotspot_type != 'none',
            Hotspot = snv_hotspot == T | threeD_hotspot == T | indel_hotspot == T)

    maf
}
