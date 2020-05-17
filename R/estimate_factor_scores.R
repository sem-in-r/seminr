#' seminr estimate_lavaan_ten_berge() function
#'
#' Estimates factor scores using ten Berge method for a fitted Lavaan model
#'
#' @param fit A fitted \code{lavaan} object – can be extracted from cbesem estimation or from using Lavaan directly.
#'
#' @return A list with two elements: ten berge scores; weights for calculating scores
#'
#' @examples
#' cbsem <- estimate_cbsem(...)
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
