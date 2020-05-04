# function to call to get scores, only argument is lavaan model called `fit` here
# returns list with two elements:
# ten berge scores and weights for calculating scores
estimate_lavaan_ten_berge <- function (fit) {
  X <- lavaan::lavInspect(fit, "data")
  i.means <- fit@SampleStats@mean[[1]]
  i.sds <- sqrt(fit@SampleStats@var[[1]])
  Lambda_mat <- lavaan::lavInspect(fit, what = "std.lv")$lambda
  Phi_mat <- matrix(lavaan::lavInspect(fit, what = "cor.lv"), ncol(Lambda_mat))
  calc_ten_berge_scores(X, Lambda_mat, Phi_mat, i.means, i.sds)
}
