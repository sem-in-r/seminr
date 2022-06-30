#' Standardize (scale) a matrix/df and report interpretable errors
#'
#' @param x vector, data.frame, or matrix
#' @return scaled object as returned by \code{scale} function
standardize_safely <- function(x) {
  # NOTE: we could return zeros for columns with zero variance:
  # apply(x, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
  res <- scale(x, TRUE, TRUE)
  if (any(is.nan(res))) stop("zero variance items cannot be scaled")
  res
}
