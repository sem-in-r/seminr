#' Structural specification functions for seminr package
#'
#' \code{paths} creates the structural paths of a PLS SEM model and \code{relationships} generates
#' the matrix of paths.
#'
#' @param ... A comma separated list of all the structural relationships in the the model. These
#'  paths take the form (from = c(construct_name), to = c(construct_name)).
#'
#' @param to The destination construct of a structural path
#'
#' @param from The source construct of a structural path
#'
#' @param paths The function \code{paths} that specifies the source and destination constructs
#'   for each of the model's structural paths.
#'
#' @usage
#' relationships(...)
#'
#' paths(from,to)
#'
#' @examples
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' @aliases paths
#'
#' @export
relationships <- function(...) {
  smMatrix <- matrix(c(...), ncol = 2, byrow = TRUE,
                     dimnames = list(NULL, c("source", "target")))
  class(smMatrix) <- c(class(smMatrix), "structural_model")
  return(smMatrix)
}
#' @export
paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

