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
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interactions}}
#'          \code{\link{bootstrap_model}}
#'
#' @examples
#' data("mobi", package = "semPLS")
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_pls(data = mobi,
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
  rSquared <- plsModel$rSquared

  # Calculate rhoA for adjustments and adjust the correlation matrix
  rho <- rhoA(plsModel)
  adjustment <- sqrt(rho %*% t(rho))
  diag(adjustment) <- 1
  adj_fscore_cors <- cor(plsModel$fscores) / adjustment

  # iterate over endogenous latents and adjust path coefficients and R-squared
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

    # adjust the Rsquared of the endogenous latents
    r_sq <- 1 - 1/solve(adj_fscore_cors[c(exogenous,i),c(exogenous,i)])
    rSquared[1,i] <- r_sq[i,i]
    rSquared[2,i] <- 1 - (1 - rSquared[1,i])*((nrow(plsModel$data)-1)/(nrow(plsModel$data)-length(exogenous) - 1))
  }

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
  plsModel$rSquared <- rSquared
  return(plsModel)
}
