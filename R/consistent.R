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
  # Calculate PLSc function
  # get smMatrix and dependant latents, rhoA
  smMatrix <- plsModel$smMatrix
  mmMatrix <- plsModel$mmMatrix
  dependant <- unique(smMatrix[,"target"])
  rho <- rhoA(plsModel)
  latents <- unique(as.vector(unique(smMatrix)))
  path_coef <- plsModel$path_coef
  weights <- plsModel$outer_weights
  loadings <- plsModel$outer_loadings

  # Determine inconsistent lv correlations from simplePLS
  latentscores <- plsModel$fscores
  oldcor <- cor(latentscores,latentscores)

  # adjust all the latents
  for(i in latents) {
    for(j in latents) {
      if(i == j) {
        oldcor[i,j] <- 1
      }
      if (i != j) {
        oldcor[i,j] <- oldcor[i,j]/(sqrt(rho[i,1]*rho[j,1]))
      }
    }
  }

  for (i in dependant)  {

    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==i,"source"]

    #Solve the sistem of equations
    results<- solve(oldcor[independant,independant],
                    oldcor[independant,i])

    #Transform to a generic vector
    coefficients <- as.vector(results)
    if(!is.null(rownames(results))) {
      names(coefficients)<-rownames(results)
    } else if (!is.null(names(results))) {
      names(coefficients)<-names(results)
    } else {
      names(coefficients)<-independant
    }
    #Assign the Beta Values to the Path Coefficient Matrix
    for (j in independant) {
      path_coef[j,i] <- coefficients[j]
    }

  }

  # adjust for consistent loadings of common factors
  for (i in rownames(rho))  {
    # if the latent is reflective
    if(measure_mode(i,mmMatrix)=="R") {
      # get the weights for the latent
      w <- as.matrix(weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])
      loadings[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i] <- w %*% (sqrt(rho[i,]) / t(w) %*% w )
    }
  }

  plsModel$path_coef <- path_coef
  plsModel$outer_loadings <- loadings
  return(plsModel)
}
# end PLSc function
