#' seminr rhoA Function
#'
#' The \code{rhoA} function calculates the rhoA reliability indices for each construct. For
#' formative constructs, the index is set to 1.
#'
#' @param plsModel A \code{seminr_model} containing the estimated seminr model.
#'
#' @usage
#' rhoA(seminr_model)
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
#' mobi_pls <- estimate_pls(data = mobi,
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#'
#' rhoA(mobi_pls)
#' @export
rhoA <- function(plsModel) {
  # get latent variable scores and weights for each latent
  latentscores <- plsModel$fscores
  weights <- plsModel$outer_weights
  # get the mmMatrix and smMatrix
  mmMatrix <- plsModel$mmMatrix
  smMatrix <- plsModel$smMatrix
  obsData <- plsModel$data
  # Create rhoA holder matrix
  rho <- matrix(,nrow = ncol(latentscores),ncol = 1,dimnames = list(colnames(latentscores),c("rhoA")))

  for (i in rownames(rho))  {
    #If the measurement model is Formative assign rhoA = 1
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="F"){
      rho[i,1] <- 1
    }
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="C"){
      rho[i,1] <- 1
    }
    #If the measurement model is Reflective Calculate RhoA
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="R"){
      #if the latent is a single item rhoA = 1
      if(nrow(mmMatrix_per_latent(i,mmMatrix)) == 1) {
        rho[i,1] <- 1
      } else {
        # get the weights for the latent
        w <- as.matrix(weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])

        # Get empirical covariance matrix of lv indicators (S)
        indicators <- scale(obsData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]],TRUE,TRUE)
        S <- cov(indicators,indicators)
        diag(S) <- 0

        # Get AA matrix without diagonal
        AAnondiag <- w %*% t(w)
        diag(AAnondiag) <- 0

        # Calculate rhoA
        rho[i,1] <- (t(w) %*% w)^2 * ((t(w) %*% (S) %*% w)/(t(w) %*% AAnondiag %*% w))
      }
    }
  }
  return(rho)
}
# End rhoA function
