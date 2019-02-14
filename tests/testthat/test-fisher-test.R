context('fisher_test')

test_that('returns valid output', {
    expect_is(fisher_test(1, 2, 3, 4), 'data.frame')
    expect_identical(names(fisher_test(1, 2, 3, 4)),
                     c('estimate', 'p.value', 'conf.low', 'conf.high', 'method', 'alternative'))
})

test_that('returns error', {
    expect_error(fisher_test(1, 2, 3))
})
