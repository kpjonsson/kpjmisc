#' \code{ggplot2} themes
#'
#' \code{theme_bw} is a simple black-and white theme for interactive plotting.
#' \code{theme_pub} is a theme for producing publication-ready plots, with appropriate font and line sizing.
#
#' @return None
#'
#' @importFrom ggplot2 theme_set theme theme_bw ggplot
#'
#' @examples
#' library(ggplot2)
#' theme_set(theme_bwmin)
#' ggplot() + theme_pub
#'
#' @name ggplot2_themes
NULL

#' @export
#' @rdname ggplot2_themes
theme_bwmin = theme_bw() +
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

#' @export
#' @rdname ggplot2_themes
theme_pub = theme_bw() +
    theme(
        text = element_text(family = 'ArialMT', size = 6),
        axis.text = element_text(family = 'ArialMT', size = 6, color = 'black'),
        line = element_line(size = .75*(.75/1.6), lineend = 'round'),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = 'black', size = .75*(.75/1.6), lineend = 'round'),
        axis.line.y = element_line(color = 'black', size = .75*(.75/1.6), lineend = 'round'),
        axis.ticks.length = unit(2, 'pt'),
        axis.ticks.x = element_line(color = 'black', size = .75*(.75/1.6), lineend = 'round'),
        axis.ticks.y = element_line(color = 'black', size = .75*(.75/1.6), lineend = 'round'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(face = 'bold'),
        panel.background = element_blank(),
        plot.margin = unit(c(0.25,0.5,0.25,0.25), 'lines')
    )
