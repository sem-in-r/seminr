#' Function to clean data of ommitted values by mean replacement
#'
#' The \code{seminr} package provides a natural syntax for researchers to describe PLS
#' structural equation models.
#' \code{mean_replacement} provides the verb for replacing all ommitted values in the dataset with
#' the mean of the variable.
#'
#' @param data A dataset to be used for estimating a SEMinR model
#'
#' @param missing_value_ind The indicator to be used to indicate which values are missing. E.g -99, "NA" or "" are often used.
#'
#' @usage
#' mean_replacement(data, missing_value_ind = -99)
#'
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares
#'  Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
#'
#' @export
mean_replacement <- function(data, missing_value_ind) {
  for (i in 1:ncol(data)) {
    colmean <- mean(data[,i][!(data[,i] == missing_value_ind)])
    data[,i][(data[,i] == missing_value_ind)] <- colmean
  }
  return(data)
}
