#' Minimalistic ggplot2 theme with font and line sizes for publication
#'
#' Use by adding \code{+theme_bwmin()} to your ggplot call.

library(ggplot2)

theme_pub = theme_bw() +
    theme(
        text = element_text(family = 'ArialMT', size = 6),
        axis.text = element_text(family = 'ArialMT', size = 6, color = 'black'),
        line = element_line(size = .75*0.352777778, lineend = 'round'),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = 'black', size = .75*0.352777778, lineend = 'round'),
        axis.line.y = element_line(color = 'black', size = .75*0.352777778, lineend = 'round'),
        axis.ticks.length = unit(2, 'pt'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.25, "cm"),
        axis.ticks = element_line(color = 'black', size = .75*0.352777778, lineend = 'round'),
        legend.title = element_text(face = 'bold'),
        panel.background = element_blank(),
        plot.margin = unit(c(0.25,0.5,0.25,0.25), 'lines')
    )
