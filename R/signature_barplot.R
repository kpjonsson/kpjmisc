signature_barplot = function(
  sign,
  col_vec = NULL,
  sample_order = NULL,
  sign_order = NULL
) {
    sign = select(sign, -`Number of Mutations`, sample_id = `Sample Name`) %>%
        reshape2::melt(id.vars = 'sample_id') %>%
        mutate(variable = stringr::str_replace(variable, 'Signature.', ''))

    if (!is.null(sample_order)) sign$sample_id = factor(sign$sample_id, levels = sample_order, ordered = T)

    if (!is.null(sign_order)) { sign$variable = factor(sign$variable, levels = sign_order, ordered = T)
    } else { sign$variable = factor(sign$variable, levels = seq(1,30), ordered = T) }

    if (is.null(col_vec)) {
        grey_pal = colorRampPalette(brewer.pal(9, 'Greys'))(16)
        col_vec = c("#e41a1c","#377eb8","#4daf4a","#984ea3",grey_pal[2],"#ff7f00","#ffff33",grey_pal[3],"#a65628",
                    "#f781bf","#b2df8a",grey_pal[4],"#377eb8",grey_pal[5],"#ff7f00",grey_pal[6:9],"#ff7f00",
                    grey_pal[10],"#cab2d6",grey_pal[11:13],"#ff7f00",grey_pal[14:15],"#984ea3",grey_pal[16])
    }

    ggplot(sign, aes(x = sample_id, fill = value)) +
        geom_bar(aes(weight = value, fill = variable)) +
        theme_bwmin() +
        scale_fill_manual(values = col_vec) +
        theme(legend.key = element_blank(), axis.text.y = element_text(size = 6), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        guides(fill = guide_legend(keywidth = .5, keyheight = .5, ncol = 2, title = 'Signature')) +
        ylab('Fraction of mutations') +
        xlab('') +
        scale_y_continuous(expand = c(0,0))
}
