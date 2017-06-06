#' Minimalistic ggplot2 theme with font and line sizes for publication
#'
#' Use by adding \code{+theme_bwmin()} to your ggplot call.

library(ggplot2)

theme_pub = theme_bwmin() +
    theme(text = element_text(family = 'Helvetica', size = 6),
          axis.text = element_text(family = 'Helvetica', size = 6, color = 'black'),
          line = element_line(size = .25, lineend = 'round'),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black', size = .25),
          axis.ticks = element_line(size = .25),
          legend.title = element_text(face = 'bold'))
