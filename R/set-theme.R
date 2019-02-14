#' Set \code{ggplot2} theme
#'
#' Basic function to set \code{ggplot2} theme, also setting default fonts for all text objects
#'
#' @param user_theme Name of theme
#'
#' @importFrom ggplot2 scale_color_manual scale_fill_manual update_geom_defaults theme_set

#' @export
set_theme = function(user_theme) {

    theme_set(user_theme)

    # update fonts for text/labels
    theme_font = user_theme$text$family
    update_geom_defaults('text', list(family = theme_font))
    update_geom_defaults('label', list(family = theme_font))

    my_pal = c(
        '#444444',
        '#de6757',
        '#EB9050',
        '#3262AB',
        '#FF8D7D',
        '#C8E370',
        '#C45B4D',
        '#41a65c',
        '#5E2C25',
        '#78695F'
    )

    # Change default colors
    assign('scale_colour_discrete', function(..., values = my_pal)
        scale_color_manual(..., values = values), globalenv())
    assign('scale_fill_discrete', function(..., values = my_pal)
        scale_fill_manual(..., values = values), globalenv())
    assign('scale_colour_gradient', function(..., low = '#C8E370', high = '#de6757')
        scale_fill_manual(..., values = values), globalenv())
    assign('scale_colour_gradient2', function(..., low = '#C8E370', high = '#de6757', mid = 'white')
        scale_fill_manual(..., values = values), globalenv())


}

