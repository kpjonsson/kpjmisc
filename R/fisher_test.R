#' Fisher's test
#'
#' Wraps \code{fisher.test} and tidies the output with \code{broom}
#'
#' @param a,a_total,b,b_total Test groups A and B
#' @param alternative \code{two.sided} \code{greater} or \code{less}
#'
#' @return Test statistics
#'
#' @importFrom broom tidy

#' @export
fisher_test = function(a_positive, a_negative,
                       b_positive, b_negative,
                       alternative = 'two.sided',
                       conf_level = 0.95) {

    test_data = matrix(c(a_positive, a_negative,
                         b_positive, b_negative),
                       ncol = 2)

    fisher.test(test_data,
                alternative,
                conf.level = conf_level) %>%
        tidy()
}
