#' Function to clean data of omitted values by mean replacement
#'
#' The \code{seminr} package provides a natural syntax for researchers to describe PLS
#' structural equation models.
#'
#' \code{mean_replacement} provides the verb for replacing all omitted values (NA only) in the dataset with
#' the mean of the variable.
#'
#' @param data A dataset to be used for estimating a SEMinR model
#'
#' @return A dataset with all missing values replaced with column means
#'
#' @usage
#' mean_replacement(data)
#'
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares
#'  Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
#'
#' @export
mean_replacement <- function(data) {
  for (i in 1:ncol(data)) {
    colmean <- mean(data[,i][!(is.na(data[,i]))])
    data[,i][is.na(data[,i])] <- colmean
  }
  return(data)
}
