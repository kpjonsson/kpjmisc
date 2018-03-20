#' Annotate read MAF with hotspots
#'
#' Adds hotspot annotation to VEP-annotated MAF. Sources of default hotspots below.
#'
#' @param maf Input MAF
#' @param hotspot Custom list of hotspots
#'
#' @return Annotated MAF with columns \code{snv_hotspot}, \code{threeD_hotspot}, \code{indel_hotspot_type} and \code{Hotspot} indicating types of hotspots
#'
#'
#' @source \url{cancerhotspots.org}
#' @source \url{3dhotspots.org}
#' @source \url{www.ncbi.nlm.nih.gov/pubmed/26619011}
#'
#' @name hotspot_annotate_maf
NULL

# Source of hotspots
hs_from_local = c('/luna/work/hotspots/24k/hotspots_24k_FULL.txt',
                  '/luna/work/hotspots/nbt/hotspots_NBT_FULL.txt',
                  '/luna/work/hotspots/3d/hotspots.txt')

hs_from_cluster = c('/ifs/work/taylorlab/jonssonp/hotspots/24k/hotspots_24k_FULL.txt',
                    '/ifs/work/taylorlab/jonssonp/hotspots/nbt/hotspots_NBT_FULL.txt',
                    '/ifs/work/taylorlab/jonssonp/hotspots/3d/hotspots.txt')

if (Sys.info()[['nodename']] == 'lski2423') {
    hotspot_files =  hs_from_local
    } else {
    hotspot_files = hs_from_cluster
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
    if (is.null(hotspots)) {
        hotspots = map_dfr(hotspot_files, fread) %>%
            mutate(indel_hotspot = Type == 'in-frame indel',
                   indel_hotspot = ifelse(is.na(indel_hotspot), F, indel_hotspot),
                   threeD_hotspot = ifelse(Class %in% c('Hotspot-linked', 'Cluster-exclusive'), T, F),
                   snv_hotspot = indel_hotspot == F & threeD_hotspot == F,
                   Pos = ifelse(indel_hotspot == F, str_extract(Residue, '(?<=[A-Z])[0-9]+'), NA),
                   Start = ifelse(indel_hotspot == T, str_extract(Residue, '^[0-9]+(?=-)'), NA),
                   End = ifelse(indel_hotspot == T, str_extract(Residue, '(?<=-)[0-9]+$'), NA),
                   Pos = ifelse(indel_hotspot == T & is.na(Start), Residue, Pos)) %>%
            distinct(Gene, Residue, snv_hotspot, indel_hotspot, threeD_hotspot, Pos, Start, End) %>%
            mutate(tag = str_c(Gene, Pos),
                   Pos = as.numeric(Pos),
                   Start = as.numeric(Start),
                   End = as.numeric(End))
    }

    # Function that deals with indel hotspots
    tag_indel_hotspot = function(gene, start, end) {
        is_hotspot = 'none'
        if (end - start <= 5) { # if short hotspot overlapping known hotspot
            snv_hs = filter(hotspots, Gene == gene & (indel_hotspot == F | is.na(End))) %>% # includes indel hotspots that only cover one codon
                filter(between(Pos, start, end))
            if (nrow(snv_hs) > 0) is_hotspot = 'novel'
        }
        indel_hs = filter(hotspots, Gene == gene, indel_hotspot == T) %>%
            filter(Start <= start & End >= end)
        if (nrow(indel_hs) > 0) is_hotspot = 'prior'
        return(is_hotspot)
    }

    maf = mutate(maf,
                 residue = str_extract(Protein_position, '^[0-9]+(?=/|-)'),
                 start_residue = residue,
                 end_residue = str_extract(Protein_position, '(?<=-)[0-9]+(?=/)'),
                 end_residue = ifelse(is.na(end_residue) & Variant_Classification %like% 'In_Frame', start_residue, end_residue)) %>%
        rowwise() %>%
        mutate(
            start_residue = as.numeric(start_residue),
            end_residue = as.numeric(end_residue),
            snv_hotspot = ifelse(Variant_Type == 'SNP' & Variant_Classification %in% coding_mutations & !is.na(residue),
                                 str_c(Hugo_Symbol, residue) %in% hotspots$tag[hotspots$snv_hotspot == T],
                                 FALSE),
            threeD_hotspot = ifelse(Variant_Type == 'SNP' & Variant_Classification %in% coding_mutations & !is.na(residue),
                                    str_c(Hugo_Symbol, residue) %in% hotspots$tag[hotspots$threeD_hotspot == T],
                                    FALSE),
            indel_hotspot_type = ifelse(Variant_Classification %like% 'In_Frame',
                                        tag_indel_hotspot(Hugo_Symbol, start_residue, end_residue),
                                        'none'),
            indel_hotspot = indel_hotspot_type != 'none',
            Hotspot = snv_hotspot == T | threeD_hotspot == T | indel_hotspot == T)

    maf
}
