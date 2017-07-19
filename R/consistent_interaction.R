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
PLSc_interact <- function(plsModel) {
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

  # create interaction term
  latentscores <- plsModel$fscores[,1:2]
  interaction <- as.matrix(scale(latentscores[,1]*latentscores[,2],center = TRUE, scale = FALSE))
  colnames(interaction) <- "interaction"
  latentscores <- cbind(latentscores,interaction)
  latentscores <- cbind(latentscores,plsModel$fscores[,3])
  colnames(latentscores)[4] <- colnames(plsModel$fscores)[3]

  # Determine inconsistent lv correlations from simplePLS
  oldcor <- cov(latentscores,latentscores)

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
  adjustment <- (((mean((plsModel$fscores[,1]*plsModel$fscores[,2])^2) - 1)/(rho[1,1]*rho[2,1]))+1) - (cor(plsModel$fscores[,1],plsModel$fscores[,2])^2/(rho[1,1]*rho[2,1]))
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

  plsModel$path_coef <- path_coef
  plsModel$outer_loadings <- loadings
  return(plsModel)
}
# end PLSc function









############### Workings

eta1 <- mobi_pls$fscores[,1]
> eta2 <- mobi_pls$fscores[,2]
> cor(eta1,eta2)
[1] 0.5098077
> cov(eta1,eta2)
[1] 0.5098077
> cor(eta1,eta1)
[1] 1
> eta1eta2 <- eta1*eta2
> mean(eta1eta2)
[1] 0.5077685
> centered_eta1eta2 <- scale(eta1eta2, center = TRUE, scale = FALSE)
> mean(eta1eta2)
[1] 0.5077685
> mean(centered_eta1eta2)
[1] 3.938972e-17
> sd(eta1eta2)
[1] 1.250534
> sd(centered_eta1eta2)
[1] 1.250534
> cor(eta1eta2,eta1eta2)
[1] 1
> cor(centered_eta1eta2,centered_eta1eta2)
[,1]
[1,]    1
> cov(centered_eta1eta2,centered_eta1eta2)
[,1]
[1,] 1.563836
> cov(eta1,centered_eta1eta2)
[,1]
[1,] -0.0131328
> eta1eta1eta2 <- eta1*eta1*eta2
> mean(eta1eta1eta2)
[1] -0.01308027
> eta1eta1eta2eta2 <- eta1*eta1*eta2*eta2
> mean(eta1eta1eta2eta2)
[1] 1.81541
> cor(eta1,eta2)
[1] 0.5098077
> cor(eta1,eta2)^2
[1] 0.2599039
> cov(centered_eta1eta2,centered_eta1eta2)
[,1]
[1,] 1.563836
> cor(centered_eta1eta2,centered_eta1eta2)
[,1]
[1,]    1
> mean(eta1eta1eta2eta2)
[1] 1.81541
> mean(eta1eta1eta2eta2) - cor(eta1,eta2)^2
[1] 1.555506
> matrix <- cbind(eta1,eta2,centered_eta1eta2)
> cov(matrix)
eta1       eta2
eta1  1.0000000  0.5098077 -0.0131328
eta2  0.5098077  1.0000000 -0.0548070
-0.0131328 -0.0548070  1.5638362
> View(matrix)
> rho <- rhoA(mobi_pls)
> View(rho)
> Q1 <- tho[1,1]
Error: object 'tho' not found
> Q1 <- rho[1,1]
> Q2 <- rho[2,1]
> Q1*Q2*Q1
[1] 0.2496068
> cov(matrix)[1,3]
[1] -0.0131328
> cov(matrix)[1,3] / Q1*Q2*Q1
[1] -0.005940672
> Q1 <- sqrt(rho[1,1])
> Q2 <- sqrt(rho[2,1])
> cov(matrix)[1,3] / Q1*Q2*Q1
[1] -0.00883276
> mean(eta1eta1eta2eta2)
[1] 1.81541
> (mean(eta1eta1eta2eta2)-1) / Q1*Q1*Q2*Q2
[1] 0.3688537
> ((mean(eta1eta1eta2eta2)-1) / Q1*Q1*Q2*Q2) + 1
[1] 1.368854
> ((mean(eta1eta1eta2eta2)-1) / Q1*Q1*Q2*Q2) + 1 - cor(eta1,eta2)^2
[1] 1.10895
> cor(eta1,eta2)
[1] 0.5098077
> cor(eta1,eta2)^2
[1] 0.2599039
> mean(eta1eta1eta2)
[1] -0.01308027
> View(rho)
> mean(eta1*eta2*eta2)
[1] -0.05458777
> mean(eta1eta1eta2eta2)
[1] 1.81541
> mean(eta1eta1eta2eta2) - cor(eta1,eta2)
[1] 1.305602
> mean(eta1eta1eta2eta2) - cor(eta1,eta2)^2
[1] 1.555506
> cor(eta1,eta2)
[1] 0.5098077
> cor(eta1,centered_eta1eta2)
[,1]
[1,] -0.01050175
> cor(eta1,eta1eta2)
[1] -0.01050175
> mean(eta1eta1eta2)
[1] -0.01308027
> sD(centered_eta1eta2)
Error: could not find function "sD"
> sd(centered_eta1eta2)
[1] 1.250534
> sd(centered_eta1eta2)^2
[1] 1.563836
> ((mean(eta1eta1eta2eta2)-1) / Q1*Q1*Q2*Q2) + 1 - cor(eta1,eta2)^2
[1] 1.10895
