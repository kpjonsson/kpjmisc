#' Black-and-white minimalistic theme for ggplot2
#'
#' Use by adding \code{+theme_bwmin()} to your ggplot call.

library(ggplot2)

theme_bwmin = theme_bw() +
    theme(
        text = element_text(family = 'Helvetica', size = 12),
        plot.background = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_line(size = 0.5, lineend = 'round'),
        axis.line.y = element_line(size = .5, color = 'black', linetype = 'solid', lineend = 'round'),
        axis.line.x = element_line(size = .5, color = 'black', linetype = 'solid', lineend = 'round')
    )

# theme_bwmin = function(
#     base_size = 12,
#     base_family = 'Helvetica',
#     ticks = TRUE)
# {
#   ret = theme_bw(base_size = base_size, base_family = base_family) +
#     theme(
#         plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         axis.line = element_blank(),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.key.size = unit(.25, "cm"),
#         panel.border = element_rect(fill = NA, colour = 'black', size = 0.5),
#         axis.ticks = element_line(size = 0.5),
#         panel.background = element_blank()
#         )
#   if (!ticks) ret = ret + theme(axis.ticks = element_blank())
#   ret
# }
