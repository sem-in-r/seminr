#' seminr fSquared Function
#'
#' The \code{fSquared} function calculates f^2 effect size for a given IV and DV
#'
#' @param seminr_model A \code{seminr_model} containing the estimated seminr model.
#' @param iv An independent variable in the model.
#' @param dv A dependent variable in the model.
#'
#' @usage
#' fsquared(model, iv, dv)
#'
#' @references Cohen, J. (2013). Statistical power analysis for the behavioral sciences. Routledge.
#'
#' @examples
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#'
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' fSquared(mobi_pls, "Image", "Satisfaction")
#' @export
fSquared <- function(seminr_model, iv, dv) {
  with_sm <- seminr_model$smMatrix
  without_sm <- subset(with_sm, !((with_sm[, "source"] == iv) & (with_sm[, "target"] == dv)))
  capture.output(
    without_pls <- estimate_pls(data = seminr_model$rawdata,
                                measurement_model = seminr_model$mmMatrix,
                                interactions = seminr_model$interactions,
                                structural_model = without_sm)
  )
  with_r2 <- seminr_model$rSquared["Rsq", dv]
  ifelse(any(without_sm[,"target"] == dv),
         without_r2 <- without_pls$rSquared["Rsq", dv],
         without_r2 <- 0)
  return((with_r2 - without_r2) / (1 - with_r2))
}
