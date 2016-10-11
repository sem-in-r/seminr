#' Functions for reporting the Path Coefficients and R2 of endogenous constructs
#' and for generating a scatterplot matrix of factor scores.
#'
#' \code{print_paths} generates an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs.\code{plot_scores} generates a
#'   scatterplot matrix of each factor's scores against every other factor's scores.
#'
#' These functions generate an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs or a scatterplot matrix of factor
#'   scores.
#'
#' @param fitted_model An object of class \code{seminr}. The estimated model
#'   returned by the \code{seminr} function.
#'
#' @param na.print A \code{character} substituting values not to be printed.
#'   If not specified, default = "."
#'
#' @param digits A \code{numeric} minimum number of significant digits. If not
#'   specified, default is "2".
#'
#' @param factors a \code{list} indicating which factors to report. If not
#'   specified, all factors are graphed and returned.
#'
#' @usage
#' print_paths(model, na.print=".", digits=2)
#' ## S3 method for class 'seminr'
#'
#' plot_scores(fitted_model, factors=NULL)
#' ## S3 method for class 'seminr'
#'
#' @examples
#' data("mobi", package = "semPLS")
#' mobi_pls <- seminr(data = mobi,
#'                    measurement_model = mobi_mm,
#'                    structural_model = mobi_sm)
#' print_paths(mobi_pls)
#' plot_scores(mobi_pls)
#'
#' @aliases plot_scores
#'
print_paths <- function(fitted_model, na.print=".", digits=2) {
  endogenous <- unique(fitted_model$model$strucmod[,"target"])
  exogenous <- unique(fitted_model$model$strucmod[,"source"])
  latent <- fitted_model$model$latent
  structure_spec <- fitted_model$model$D

  # create matrix of relevant path coefficients and NAs otherewise
  path_matrix <- matrix(nrow = length(latent), ncol = length(latent), dimnames = list(latent, latent))
  path_matrix[structure_spec == 1] <- fitted_model$path_coefficients[structure_spec == 1]

  # add R Squared row
  r_sq <- t(semPLS::rSquared(fitted_model))[1, ]
  path_matrix <- rbind(r_sq, path_matrix)
  rownames(path_matrix) <- c("R^2", latent)

  # round and print
  final_paths <- round(path_matrix[c("R^2", exogenous), endogenous, drop=FALSE], digits)
  print(final_paths, na.print = na.print)
}

plot_scores <- function(fitted_model, factors=NULL) {
  if (missing(factors)) factors <- fitted_model$model$latent

  plot(as.data.frame(fitted_model$factor_scores[, factors]), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
