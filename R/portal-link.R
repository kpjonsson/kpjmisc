#' cBioPortal link
#'
#' Creates URL based on listed samples to instance of MSK-IMPACT samples
#'
#' @param samples Vector of one or more samples to look up.
#'
#' @return URL to sample(s) at cBioPortal
#'
#' @source \url{https://cbioportal.mskcc.org}

#' @export
portal_link = function(samples) {

    base_url = 'https://cbioportal.mskcc.org/patient?studyId=mskimpact'
    samples = unique(str_sub(samples, 1, 9)) # truncate to patient ID

    if (length(samples) > 1) {
        query_string = paste0('&caseId=', samples[1], '#navCaseIds=', paste0('mskimpact:', samples[-1], collapse = ','))
    } else {
        query_string = paste0('&caseId=', samples[1])
    }

    paste0(base_url, query_string)
}
