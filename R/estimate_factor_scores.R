#' seminr estimate_lavaan_ten_berge() function
#'
#' Estimates factor scores using ten Berge method for a fitted Lavaan model
#'
#' @param fit A fitted \code{lavaan} object – can be extracted from cbesem estimation or from using Lavaan directly.
#'
#' @return A list with two elements: ten berge scores; weights for calculating scores
#'
#' @examples
#' #' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   reflective("Image",        multi_items("IMAG", 1:5)),
#'   reflective("Quality",      multi_items("PERQ", 1:7)),
#'   reflective("Value",        multi_items("PERV", 1:2)),
#'   reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'   reflective("Complaints",   single_item("CUSCO")),
#'   reflective("Loyalty",      multi_items("CUSL", 1:3))
#' )
#'
#' #seminr syntax for freeing up item-item covariances
#' mobi_am <- associations(
#'   item_errors(c("PERQ1", "PERQ2"), "IMAG1")
#' )
#'
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = c("Image", "Quality"), to = c("Value", "Satisfaction")),
#'   paths(from = c("Value", "Satisfaction"), to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' # Estimate model and get results
#' cbsem <- estimate_cbsem(mobi, mobi_mm, mobi_sm, mobi_am)
#' tb <- estimate_lavaan_ten_berge(cbsem$lavaan_model)
#' tb$scores
#' tb$weights
#'
#' @export
estimate_lavaan_ten_berge <- function (fit) {
  X <- lavaan::lavInspect(fit, "data")
  i.means <- fit@SampleStats@mean[[1]]
  i.sds <- sqrt(fit@SampleStats@var[[1]])
  Lambda_mat <- lavaan::lavInspect(fit, what = "std.lv")$lambda
  Phi_mat <- matrix(lavaan::lavInspect(fit, what = "cor.lv"), ncol(Lambda_mat))
  calc_ten_berge_scores(X, Lambda_mat, Phi_mat, i.means, i.sds)
}
