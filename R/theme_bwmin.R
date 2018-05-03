#' \code{ggplot2} theme
#'
#' Simple black-and-white theme for \code{ggplot2}
#
#' @return None
#'
#' @examples
#' theme_set(theme_bwmin)
#' ggplot() + theme_bwmin
#'
#' @import ggplot2

#' @export
# theme_bwmin = theme_bw() +
theme_bwmin = theme_bw() %+replace%
    theme(
        text = element_text(family = 'ArialMT', size = 12, color = 'black'),
        axis.text = element_text(family = 'ArialMT', size = 12, color = 'black'),
        strip.background = element_blank(),
        strip.text.x = element_text(family = 'ArialMT', size = 12),
        strip.text.y = element_text(family = 'ArialMT', size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.75, 'lines'),
        panel.grid.major = element_line(color = 'grey92', size = .5),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(size = .75, color = 'black', linetype = 'solid', lineend = 'round'),
        axis.line.x = element_line(size = .75, color = 'black', linetype = 'solid', lineend = 'round'),
        axis.ticks.y = element_line(color = 'black', size = .75, lineend = 'round'),
        axis.ticks.x = element_line(color = 'black', size = .75, lineend = 'round'),
        axis.ticks.length = unit(2, 'pt'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')
    )
