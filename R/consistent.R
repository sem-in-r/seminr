#' seminr PLSc Function
#'
#' The \code{PLSc} function calculates the consistent PLS path coefficients and loadings for
#' a common factor model. It returns a \code{seminr.model} containing the adjusted and consistent
#' path coefficients and loadings for common factor models and composite models.
#'
#' @param seminr.model A \code{seminr.model} containing the estimated seminr model.
#'
#' @usage
#' PLSc(seminr.model)
#'
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interactions}}
#'          \code{\link{bootstrap_model}}
#'
#' @examples
#' mobi <- mobi
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
#' seminr.model <- estimate_pls(data = mobi,
#'                              measurement_model = mobi_mm,
#'                              structural_model = mobi_sm)
#'
#' PLSc(seminr.model)
#' @export
PLSc <- function(seminr.model) {
  # get relevant parts of the estimated model
  smMatrix <- seminr.model$smMatrix
  mmMatrix <- seminr.model$mmMatrix
  path_coef <- seminr.model$path_coef
  loadings <- seminr.model$outer_loadings
  rSquared <- seminr.model$rSquared
  fscores <- seminr.model$fscores

  # Calculate rhoA for adjustments and adjust the correlation matrix
  rho <- rhoA(seminr.model)
  adjustment <- sqrt(rho %*% t(rho))
  diag(adjustment) <- 1
  adj_fscore_cors <- stats::cor(seminr.model$fscores) / adjustment

  # iterate over endogenous latents and adjust path coefficients and R-squared
  for (i in unique(smMatrix[,"target"]))  {

    #Indentify the exogenous variables
    exogenous<-smMatrix[smMatrix[,"target"]==i,"source"]

    #Solve the system of equations
    results <- solve(adj_fscore_cors[exogenous,exogenous],
                    adj_fscore_cors[exogenous,i])
    # Assign the path names
    names(results) <- exogenous

    #Assign the Beta Values to the Path Coefficient Matrix
    path_coef[exogenous,i] <- results
  }

  #calculate insample metrics
  rSquared <- calc.insample(seminr.model$data, fscores, smMatrix, unique(smMatrix[,"target"]),adj_fscore_cors)

  # get all common-factor latents (Mode A Consistent) in a vector
  reflective <- unique(mmMatrix[mmMatrix[,"type"]=="C", "latent"])

  # function to adjust the loadings of a common-factor
  adjust_loadings <- function(i) {
    w <- as.matrix(seminr.model$outer_weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])
    loadings[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i] <- w %*% (sqrt(rho[i,]) / t(w) %*% w )
    loadings[,i]
  }

  # apply the function over common-factors and assign to loadings matrix
  if(length(reflective) > 0) {
    loadings[,reflective] <- sapply(reflective, adjust_loadings)
  }

  # Assign the adjusted values for return
  seminr.model$path_coef <- path_coef
  seminr.model$outer_loadings <- loadings
  seminr.model$rSquared <- rSquared
  return(seminr.model)
}

model_consistent <- function(seminr_model) {
  if(!is.null(seminr_model$mobi_xm) && ("C" %in% seminr_model$mmMatrix[,"type"])) {
    cat("Models with interactions cannot be estimated as PLS consistent and therefore no adjustment for PLS consistent has been made\n")
  }
  if(is.null(seminr_model$mobi_xm) && ("C" %in% seminr_model$mmMatrix[,"type"])) {
    seminr_model <- PLSc(seminr_model)
  }
  return(seminr_model)
}
