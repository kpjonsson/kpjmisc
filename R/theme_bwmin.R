#' Black-and-white minimalistic theme for ggplot2
#'
#' Use by adding \code{+theme_bwmin()} to your ggplot call.

theme_bwmin = function(base_size = 12, base_family = 'Helvetica') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
          panel.grid.major = element_blank(),
          panel.border = element_rect(fill = NA, colour = 'black', size = 0.5),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = NA, colour = NA),
          axis.line = element_blank(),
          axis.ticks = element_line(size = 0.5)
          )
}
