# Power operator for matrices
"%^%" <- function(S, power) with(eigen(S), vectors %*% (values ^ power * t(vectors)))

# ten Berge factor score calculator
calc_ten_berge_scores <- function(X, Lambda, Phi, i.means, i.sds) {
  if (any(is.na(X))) {
    # if any missing, impute using person average
    p.means <- rowMeans(X, na.rm = TRUE)
    missings <- which(is.na(X), arr.ind = TRUE)
    X[is.na(X)] <- p.means[missings[, 1]]
    X <- scale(X)
  } else {
    X <- t((t(X) - i.means) / i.sds)
  }
  R <- stats::cor(X, use = "pairwise")
  R.sqrt.i <- R %^% -0.5
  Phi.sqrt <- Phi %^% 0.5
  L <- Lambda %*% Phi.sqrt
  C <- R.sqrt.i %*% L %*% ((t(L) %*% chol2inv(chol(R)) %*% L) %^% -0.5)
  W <- R.sqrt.i %*% C %*% Phi.sqrt
  colnames(W) <- colnames(Lambda)
  rownames(W) <- rownames(Lambda)
  scores <- X %*% W
  colnames(scores) <- colnames(Lambda)
  list(scores = scores, weights = W)
}
