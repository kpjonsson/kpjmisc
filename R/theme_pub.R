#' Minimalistic ggplot2 theme with font and line sizes for publication
#'
#' Use by adding \code{+theme_bwmin()} to your ggplot call.

library(ggplot2)

theme_pub = theme_bw() +
    theme(
        text = element_text(family = 'ArialMT', size = 6),
        axis.text = element_text(family = 'ArialMT', size = 6, color = 'black'),
        line = element_line(size = .75, lineend = 'round'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = 'black', size = .75),
        axis.line.y = element_line(color = 'black', size = .75),
        axis.ticks.length = unit(2, 'pt'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.25, "cm"),
        axis.ticks = element_line(size = .75),
        legend.title = element_text(face = 'bold'),
        panel.background = element_blank()
    )
