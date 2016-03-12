### List of hotspot alleles generated thusly:
# hotspots = read_tsv('~/Desktop/Misc/hotspot.v11.mutations.filtered.txt') %>% filter(judgement == 'RETAIN') %>% mutate(var = strsplit(var, '\\|')) %>%
#     tidyr::unnest(var) %>% select(gene, ref, var, aa_position, judgement) %>% mutate(ref = str_replace(ref, ':[0-9]*', '')) %>% mutate(var = str_replace(var, ':[0-9]*', '')) %>%
#     mutate(mutation = str_c('p.', ref, aa_position, var)) %>% select(Hugo_Symbol = gene, HGVSp_Short = mutation) %>% mutate(Hotspot = TRUE)
# write.table(hotspots, file = 'data/hotspots.txt', col.names = T, row.names = F, sep = '\t', quote = F)

add_hotspots_to_maf = function(maf) {
    data(hotspots)
    maf = merge(maf, hotspots, by = c('Hugo_Symbol', 'HGVSp_Short'), all.x = TRUE)
    if ('Hotspot' %in% names(maf)) stop('Hotspot column already in MAF.')
    maf$Hotspot[is.na(maf$Hotspot)] = FALSE
    maf
}


