#' Estimate somatic variant allele fraction (VAF)
#'
#' @description
#' Given copy-number and purity estimates, calculates the two expected alllele fractions of a mutation in a tumor.
#' Use \code{expected_vaf_germline} for germline mutations and \code{expected_vaf_somatic} for somatic mutations
#'
#' @param maf Input MAF
#' @param hotspot Custom list of hotspots
#'
#' @return Object containing expected VAFs if the tumor is on the major (\code{mcn}) or minor allele (\code{lcn}), respectively.
#'

#' @export
#' @rdname export_vaf
expected_vaf_germline = function(purity, mcn, lcn) {

    tcn = mcn + lcn # total number of alleles in tumor
    tumor_normal_alleles = purity*tcn + (1-purity)*2 # total number of alleles in sample

    mcn_mut_alleles = (purity*mcn + (1-purity)) # number of mutated alleles in tumor if mcn, plus mut alleles in normal cells
    lcn_mut_alleles = (purity*lcn + (1-purity)) # number of mutated alleles in tumor if lcn, plus mut alleles in normal cells

    c(vaf_mcn = mcn_mut_alleles/tumor_normal_alleles, vaf_lcn = lcn_mut_alleles/tumor_normal_alleles)
}

#' @export
#' @rdname export_vaf
expected_vaf_somatic = function(purity, mcn, lcn) {

    tcn = mcn + lcn # total number of alleles in tumor
    tumor_normal_alleles = purity*tcn + (1-purity)*2 # total number of alleles in sample

    mcn_mut_alleles = purity*mcn # number of mutated alleles in tumor if mcn, plus mut alleles in normal cells
    lcn_mut_alleles = purity*lcn # number of mutated alleles in tumor if lcn, plus mut alleles in normal cells

    c(vaf_mcn = mcn_mut_alleles/tumor_normal_alleles, vaf_lcn = lcn_mut_alleles/tumor_normal_alleles)
}
