#' @export
setlist = function(a, b, output = F) {
    a = unique(a)
    b  = unique(b)

    ab = intersect(a, b)
    a_not_b = setdiff(a, b)
    b_not_a = setdiff(b, a)

    cat(paste0(
        'Unique items A: ', length(a), '\n',
        'Unique items B: ', length(b), '\n',
        'Intersect: ', length(ab), '\n',
        'Unique to A: ', length(a_not_b), '\n',
        'Unique to B: ', length(b_not_a), '\n'
    ))

    if(output) list('A' = a, 'B', b, 'AB' = ab, 'A_not_B' = a_not_b, 'B_not_A' = b_not_a)
}
