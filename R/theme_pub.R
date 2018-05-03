#' \code{ggplot2} theme
#'
#' Simple almost-publication-ready theme for \code{ggplot2}
#
#' @return None
#'
#' @examples
#' theme_set(theme_pub)
#' ggplot() + theme_pub

#' @export
# theme_pub = theme_bw() +
theme_pub = theme_bw() %+replace%
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
        axis.ticks.x = element_line(color = 'black', size = .75*0.352777778, lineend = 'round'),
        axis.ticks.y = element_line(color = 'black', size = .75*0.352777778, lineend = 'round'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(face = 'bold'),
        panel.background = element_blank(),
        plot.margin = unit(c(0.25,0.5,0.25,0.25), 'lines')
    )
