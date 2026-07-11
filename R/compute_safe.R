#' Standardize (scale) a matrix/df and report interpretable errors
#'
#' @param x vector, data.frame, or matrix
#' @return scaled object as returned by \code{scale} function
standardize_safely <- function(x) {
  # NOTE: we could return zeros for columns with zero variance:
  # apply(x, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
  x <- as.matrix(x)
  if (anyNA(x)) {
    # scale() handles per-column NA counts in its divisor; keep it for NA data
    res <- scale(x, TRUE, TRUE)
  } else {
    # same arithmetic as scale(x, TRUE, TRUE) without sweep()/apply() overhead
    n <- nrow(x)
    center <- colMeans(x)
    res <- x - rep(center, each = n)
    scl <- sqrt(colSums(res * res) / max(1L, n - 1L))
    res <- res / rep(scl, each = n)
    attr(res, "scaled:center") <- center
    attr(res, "scaled:scale") <- scl
  }
  if (any(is.nan(res))) stop("zero variance items cannot be scaled")
  res
}
