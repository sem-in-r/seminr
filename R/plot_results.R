#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}


#' Plot Reliability can be used to generate simple and effective bar plots for reliability stats.
#'
#' The \code{plot_reliability} function generates a bar plot of reliability measures with a specified cut-off.
#'
#' @param summary_object A \code{summary.seminr_model} containing the summarized seminr model.
#'
#' @param criteria The reliability criteria to be viasualizes, AVE, RhoA, RhoC, or alpha.
#'
#' @param limit The upper limit for reliability such as 0.7 for RhoA and 0.5 for AVE.
#'
#' @usage
#' plot_reliability(sum_mobi_pls, "rhoA", 0.7))
#'
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares
#'  Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
#'
#' @examples
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
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
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#'
#' sum_mobi_pls <- summary(mobi_pls)
#' plot_reliability(sum_mobi_pls, "rhoA", 0.7)
#'
#' @export
plot_reliability <- function(summary_object, criteria, limit) {
  vec <- summary_corp_rep$reliability[,criteria]
  barplot(vec,
          col = ifelse(vec > limit, "green", "red"),
          main = criteria,
          panel.first = grid())
  abline(h = limit, lty = 2, col = "blue")
}

#' fl_criteria_table can be used to generate simple and effective table for checking Fornel Larcker criteria.
#'
#' The \code{fl_criteria_table} function generates a table of correaltions and AVE values for inspection.
#'
#' @param summary_object A \code{summary.seminr_model} containing the summarized seminr model.
#'
#' @usage
#' fl_criteria_table(sum_mobi_pls)
#'
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares
#'  Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
#'
#' @examples
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
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
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#'
#' sum_mobi_pls <- summary(mobi_pls)
#' fl_criteria_table(sum_mobi_pls)
#'
#' @export
fl_criteria_table <- function(summary_object) {
  table <- summary_object$descriptives$correlations$constructs
  table[upper.tri(table)] <- NA
  diag(table) <- sqrt(summary_object$reliability[,"AVE"])
  return(table)
}
