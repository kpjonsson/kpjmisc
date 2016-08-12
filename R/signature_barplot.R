signature_barplot = function(
  sign,
  col_vec = NULL,
  sample_order = NULL,
  sign_order = NULL,
  labels = TRUE
) {
    sign = tbl_df(sign) %>% select(-`Number of Mutations`, sample_id = `Sample Name`) %>%
        reshape2::melt(id.vars = 'sample_id') %>%
        mutate(variable = stringr::str_replace(variable, 'Signature.', ''))

    if (!is.null(sample_order)) sign$sample_id = factor(sign$sample_id, levels = sample_order, ordered = T)

    if (!is.null(sign_order)) { sign$variable = factor(sign$variable, levels = sign_order, ordered = T)
    } else { sign$variable = factor(sign$variable, levels = seq(1,30), ordered = T) }

    if (is.null(col_vec)) {

        grey_pal = colorRampPalette(RColorBrewer::brewer.pal(9, 'Greys'))(16)
        col_vec = c("#e41a1c","#377eb8","#4daf4a","#984ea3",grey_pal[2],"#ff7f00","#ffff33",grey_pal[3],"#a65628",
                    "#f781bf","#b2df8a",grey_pal[4],"#377eb8",grey_pal[5],"#ff7f00",grey_pal[6:9],"#ff7f00",
                    grey_pal[10],"#cab2d6",grey_pal[11:13],"#ff7f00",grey_pal[14:15],"#984ea3",grey_pal[16])

        sign = mutate(sign, variable = mapvalues(variable,
                                                 seq(1,30),
                                                 c('Age','APOBEC','BRCA','Smoking','5','MMR','UV','8','POLN','POLE','TMZ','12','APOBEC','14','MMR',
                                                   '16','17','18','19','MMR','21','Aristolochic acid','23','Aflatoxin','25','MMR','27','28','Chewing tobacco','30'))) %>%
            mutate(variable = factor(variable, levels = c('Age','APOBEC','BRCA','Smoking','MMR','UV','POLN','POLE','TMZ','Aristolochic acid','Aflatoxin','Chewing tobacco',
                                                          '5','8','12','14','16','17','18','19','21','23','25','27','28','30'), ordered = T))
        col_vec = c('#6baed6','#d0d1e6','#ef3b2c','#a50f15','#df65b0','#fec44f','#dadaeb','#fdbf6f','#a2d399','#f7fcb9','#fdd0a2','#cb181d', grey_pal[1:14])
    }

    sign_plot = ggplot(sign, aes(x = sample_id, fill = value)) +
        geom_bar(aes(weight = value, fill = variable)) +
        theme_bwmin() +
        scale_fill_manual(values = col_vec) +
        theme(text = element_text(size = 12), legend.key = element_blank()) +
        guides(fill = guide_legend(keywidth = .5, keyheight = .5, ncol = 2, title = 'Signature')) +
        ylab('Fraction of mutations') +
        xlab('') +
        scale_y_continuous(expand = c(0,0))

    if (labels == TRUE) sign_plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    else if (labels == FALSE) sign_plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}
