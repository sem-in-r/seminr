#' seminr PLSc Function
#'
#' The \code{PLSc} function calculates the consistent PLS path coefficients and loadings for
#' a common factor model. It returns a \code{seminr_model} containing the adjusted and consistent
#' path coefficients and loadings for common factor models and composite models.
#'
#' @param plsModel A \code{seminr_model} containing the estimated seminr model.
#'
#' @usage
#' PLSc(seminr_model)
#'
#' @seealso \code{\link{structure}} \code{\link{measure}} \code{\link{paths}} \code{\link{interact}}
#'          \code{\link{bootstrap_model}}
#'
#' @examples
#' data("mobi", package = "semPLS")
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- measure(
#'              reflect("Image",        multi_items("IMAG", 1:5)),
#'              reflect("Expectation",  multi_items("CUEX", 1:3)),
#'              reflect("Quality",      multi_items("PERQ", 1:7)),
#'              reflect("Value",        multi_items("PERV", 1:2)),
#'              reflect("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflect("Complaints",   single_item("CUSCO")),
#'              reflect("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- structure(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_model(data = mobi,
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#'
#' PLSc(mobi_pls)
#' @export
PLSc <- function(plsModel) {
  # get relevant parts of the estimated model
  smMatrix <- plsModel$smMatrix
  mmMatrix <- plsModel$mmMatrix
  path_coef <- plsModel$path_coef
  loadings <- plsModel$outer_loadings

  # Calculate rhoA for adjustments and adjust the correlation matrix
  rho <- rhoA(plsModel)
  adjustment <- sqrt(rho %*% t(rho))
  diag(adjustment) <- 1
  adj_fscore_cors <- cor(plsModel$fscores) / adjustment

  # iterate over endogenous latents and adjust path coefficients
  for (i in unique(smMatrix[,"target"]))  {

    #Indentify the exogenous variables
    exogenous<-smMatrix[smMatrix[,"target"]==i,"source"]

    #Solve the system of equations
    results<- solve(adj_fscore_cors[exogenous,exogenous],
                    adj_fscore_cors[exogenous,i])

    ## NAME THE NEWLY COMPUTED PATH COEFFICIENTS VECTOR
    coefficients <- transform_to_named_vector(results,exogenous)

    #Assign the Beta Values to the Path Coefficient Matrix
    for (j in exogenous) {
      path_coef[j,i] <- coefficients[j]
    }
  }

  ## (make sure single-item factors have rhoA == 1)

  # get all common-factor latents in a vector
  reflective <- unique(mmMatrix[mmMatrix[,"type"]=="R", "latent"])

  # function to adjust the loadings of a common-factor
  adjust_loadings <- function(i) {
    w <- as.matrix(plsModel$outer_weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])
    loadings[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i] <- w %*% (sqrt(rho[i,]) / t(w) %*% w )
    loadings[,i]
  }

  # apply the function over common-factors and assign to loadings matrix
  loadings[,reflective] <- sapply(reflective, adjust_loadings)

  # Assign the adjusted values for return
  plsModel$path_coef <- path_coef
  plsModel$outer_loadings <- loadings
  return(plsModel)
}
