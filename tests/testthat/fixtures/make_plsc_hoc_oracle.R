# Generates plsc_hoc_oracle.rds: cSEM two-stage PLSc estimates of second-order
# composites (HOCs), for the PLSc HOC tests. A composite HOC of common-factor LOCs
# is not error-free at stage 2: its proxy is a weighted sum of error-laden LOC
# scores, so cSEM corrects it with the reliability w' S* w, where S* is the
# LOC-score correlation matrix with the LOCs' stage-1 reliabilities on the
# diagonal (van Riel et al., 2017). With composite LOCs the reliability is 1.
# cSEM always re-estimates the HOC in Mode B at stage 2, so the seminr designs
# use higher_composite(..., weights = mode_B).
# Run from the package root:  Rscript tests/testthat/fixtures/make_plsc_hoc_oracle.R
# Generated with cSEM 0.6.1.9000.
suppressMessages(library(cSEM))
d <- as.data.frame(seminr::mobi)
two_stage <- function(locs, ...) {
  syntax <- paste(locs, "IE <~ Image + Expectation
    Satisfaction =~ CUSA1 + CUSA2 + CUSA3
    Loyalty =~ CUSL1 + CUSL2 + CUSL3
    Satisfaction ~ IE
    Loyalty ~ Satisfaction", sep = "\n")
  fit <- csem(d, syntax, .approach_2ndorder = "2stage", .disattenuate = TRUE,
              .tolerance = 1e-10, .PLS_weight_scheme_inner = "path", ...)
  est <- fit$Second_stage$Estimates
  list(ie_satisfaction = est$Path_estimates["Satisfaction_temp", "IE"],
       satisfaction_loyalty = est$Path_estimates["Loyalty_temp", "Satisfaction_temp"],
       rho_ie = unname(est$Reliabilities["IE"]))
}
factor_locs <- two_stage("Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation =~ CUEX1 + CUEX2 + CUEX3")
mixed_locs <- two_stage("Image =~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation <~ CUEX1 + CUEX2 + CUEX3", .PLS_modes = list(Expectation = "modeA"))
composite_locs <- two_stage("Image <~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation <~ CUEX1 + CUEX2 + CUEX3", .PLS_modes = list(Image = "modeA", Expectation = "modeA"))
saveRDS(list(factor_locs = factor_locs, mixed_locs = mixed_locs, composite_locs = composite_locs),
        "tests/testthat/fixtures/plsc_hoc_oracle.rds", version = 2)
