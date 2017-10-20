#' seminr Consistent Interaction Function
#'
#' The \code{PLSc_interact} function calculates the consistent PLS path coefficients and loadings for
#' a common factor model with a simple interaction. It returns a \code{seminr.model} containing the
#' adjusted and consistent path coefficients and loadings for a common factor model.
#'
#' @param seminr.model A \code{seminr.model} containing the estimated seminr model with only the two
#' antecedent constructs (no interaction is included yet, but will be automatically created using
#' the two antecedent constructs).
#'
#' @usage
#' PLSc_interact(seminr.model)
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
#'              reflective("Satisfaction", multi_items("CUSA", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = "Satisfaction"),
#'   paths(from = "Expectation",  to = "Satisfaction")
#' )
#'
#' mobi.pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' mobi.pls.consistent.interaction <- PLSc_interact(mobi.pls)
#'
#' #path coefficients, R^2 and loadings
#' mobi.pls.consistent.interaction$path_coef
#' mobi.pls.consistent.interaction$rSquared
#' mobi.pls.consistent.interaction$outer_loadings
#'
#' @export
PLSc_interact <- function(seminr.model) {
  # Calculate PLSc function
  # get smMatrix and dependant latents, rhoA
  smMatrix <- seminr.model$smMatrix
  mmMatrix <- seminr.model$mmMatrix
  dependant <- unique(smMatrix[,"target"])
  rho <- rhoA(seminr.model)
  latents <- unique(as.vector(unique(smMatrix)))
  path_coef <- seminr.model$path_coef
  weights <- seminr.model$outer_weights
  loadings <- seminr.model$outer_loadings

  # create interaction term
  latentscores <- seminr.model$fscores[,1:2]
  interaction <- as.matrix(scale(latentscores[,1]*latentscores[,2],center = TRUE, scale = FALSE))
  colnames(interaction) <- "interaction"
  latentscores <- cbind(latentscores,interaction)
  latentscores <- cbind(latentscores,seminr.model$fscores[,3])
  colnames(latentscores)[4] <- colnames(seminr.model$fscores)[3]

  # Determine inconsistent lv correlations from simplePLS
  oldcor <- stats::cov(latentscores,latentscores)

  # create rhoA of interaction term
  interaction <- rho[1,1]*rho[2,1]
  newrho <- as.matrix(rho[1:2,1])
  newrho <- rbind(newrho,interaction)
  newrho <- rbind(newrho,rho[3,])
  rownames(newrho)[4] <- rownames(rho)[3]

  # adjust all the latents
  latents <- rownames(newrho)
  for(i in latents) {
    for(j in latents) {
      if(i == j) {
        oldcor[i,j] <- 1
      }
      if (i != j) {
        oldcor[i,j] <- oldcor[i,j]/(sqrt(newrho[i,1]*newrho[j,1]))
      }
    }
  }

  # correct for cov(eta1, centered(eta1eta2))
  adjustment <- (((mean((seminr.model$fscores[,1]*seminr.model$fscores[,2])^2) - 1)/(rho[1,1]*rho[2,1]))+1) - (stats::cor(seminr.model$fscores[,1],seminr.model$fscores[,2])^2/(rho[1,1]*rho[2,1]))
  oldcor[3,3] <- adjustment

  # adjust path_coef object
  path_coef <- matrix(0,nrow = 4,ncol = 4,dimnames = list(latents,latents))

  for (i in dependant)  {

    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==i,"source"]
    independant <- append(independant,"interaction")

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

  # adjust for consistent loadings
  for (i in rownames(rho))  {
    # get the weights for the latent
    w <- as.matrix(weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])
    loadings[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i] <- w %*% (sqrt(rho[i,]) / t(w) %*% w )
  }

  seminr.model$path_coef <- path_coef
  seminr.model$outer_loadings <- loadings
  return(seminr.model)
}
# end PLSc function
