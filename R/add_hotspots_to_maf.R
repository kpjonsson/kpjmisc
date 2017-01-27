### List of hotspot alleles generated thusly:
# hotspots = read_tsv('~/Desktop/Misc/hotspot.v11.mutations.filtered.txt') %>% filter(judgement == 'RETAIN') %>% mutate(var = strsplit(var, '\\|')) %>%
#     tidyr::unnest(var) %>% select(gene, ref, var, aa_position, judgement) %>% mutate(ref = str_replace(ref, ':[0-9]*', '')) %>% mutate(var = str_replace(var, ':[0-9]*', '')) %>%
#     mutate(mutation = str_c('p.', ref, aa_position, var)) %>% select(Hugo_Symbol = gene, HGVSp_Short = mutation) %>% mutate(Hotspot = TRUE)
# write.table(hotspots, file = 'data/hotspots.txt', col.names = T, row.names = F, sep = '\t', quote = F)

### Also add coding and truncating mutations
coding_mutations = c('Frame_Shift_Del', 'Frame_Shift_Ins', 'In_Frame_Del', 'In_Frame_Ins', 'Missense_Mutation',
                     'Nonsense_Mutation', 'Nonstop_Mutation', 'Splice_Site', 'Splice_Region', 'Targeted_Region',
                       'Translation_Start_Site')
save(coding_mutations, file = 'data/coding_mutations.rda')
truncating_mutations = c('Nonsense_Mutation', 'Frame_Shift_Ins', 'Frame_Shift_Del',
                         'Splice_Site', 'Nonstop_Mutation', 'Splice_Region')
save(truncating_mutations, file = 'data/truncating_mutations.rda')

add_hotspots_to_maf = function(maf, hotspots = NULL) {
    if (is.null(hotspots)) data(hotspots)
    if ('Hotspot' %in% names(maf)) stop('Hotspot column already in MAF.')
    maf$Residue = str_extract(maf$HGVSp_Short, '(?<=p.)[A-Z]{1}[0-9_splice]+')
    maf = base::merge(data.frame(maf), data.frame(hotspots), by = c('Hugo_Symbol', 'Residue'), all.x = TRUE)
    maf$Hotspot[is.na(maf$Hotspot)] = FALSE
    maf
}


