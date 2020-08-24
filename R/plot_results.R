#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}


#' Plot Reliability Tablecan be used to generate simple and effective bar plots for reliability stats.
#'
#' The \code{plot.reliability_table} function generates a series of bar plots of reliability measures with a specified cut-off.
#'
#' @param reliability_table A \code{reliability_table} containing the measurement model reliability of the seminr model.
#'
#' @usage
#' plot(sum_mobi_pls$reliability)
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
#' plot(sum_mobi_pls$reliability)
#'
#' @export
plot.reliability_table <- function(object) {
  stopifnot(inherits(object, "reliability_table"))
  limit <- c(0.7,0.7,0.5,0.7)
  for (i in 1:ncol(object)) {
    vec <- object[,i]
    barplot(vec,
            col = ifelse(vec > limit[i], "green", "red"),
            main = colnames(object)[i],
            panel.first = grid())
    abline(h = limit[i], lty = 2, col = "blue")
  }
}
