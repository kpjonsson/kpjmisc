#' Set \code{ggplot2} theme
#'
#' Basic function to set \code{ggplot2} theme, also setting default fonts for all text objects
#'
#' @param user_theme Name of theme

#' @export
set_theme = function(user_theme) {

    theme_set(user_theme)

    # update fonts for text/labels
    theme_font = user_theme$text$family
    update_geom_defaults('text', list(family = theme_font))
    update_geom_defaults('label', list(family = theme_font))

}

