## Goodness-of-fit ----

#' Function to calculate the SRMR of a model
#'
#' \code{SRMR} calculate the saturated and estimated SRMR of a estimated SEMinR model.
#'
#' @param seminr_model is the PLS model estimated by SEMinR to calculate SRMR for.
#'
#' @usage
#'  SRMR(seminr_model)
#'
#' @references Henseler, J., Hubona, G., & Ray, P. A. (2016). Using PLS path modeling in new technology research: updated guidelines. Industrial management & data systems, 116(1), 2-20.
#'
#' @export
SRMR <- function(seminr_model) {
  # calculate the observed correlation matrix
  obs <- stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$data[,seminr_model$mmVariables])

  # Formula Rvv = Pvf.Rff.Pfv' + Dvv
  # where Pvf is loadings matrix
  # Rff is relations matrix with diag 1
  # Pfv' is loadings matrix transposed

  # collect the loadings matrix rows = items, columns = constructs
  lambda_matrix <- seminr_model$outer_loadings

  # collect the paths matrix with a 1 diagonal
  # For saturated model:
  sat_paths_matrix <- stats::cor(seminr_model$construct_scores)

  # For regular model use the total effects matrix with a 1 diagonal:
  total_paths_matrix <- total_effects(seminr_model$path_coef) +diag(1,7,7)

  # Calculate the covariance matrix of the item errors (only within block)
  error_cov <- error_cov_matrix(seminr_model)

  # 5 calculate model implied covariance matrix for saturated and total model
  # Rvv = Pvf.Rff.Pfv' + Dvv
  sat_imp <- lambda_matrix %*% sat_paths_matrix %*% t(lambda_matrix) + error_cov
  model_imp <- lambda_matrix %*% total_paths_matrix %*% t(lambda_matrix) + error_cov

  # Calculate saturated SRMR:
  sat_SRMR <- compute_SRMR(obs,sat_imp)

  # Calculate model SRMR
  model_SRMR <- compute_SRMR(obs,model_imp)
  ## BUT this is different from Adanco, which is different from SmarTPLS!!

  return(list(Estimated_Model_SRMR = model_SRMR,
              Saturated_Model_SRMR = sat_SRMR))
}

# Calculate insample metrics ----
calc_insample <- function(obsData, construct_scores, smMatrix, dependant, construct_score_cors) {
  # matrix includes BIC
  # Remove BIC for now
  #insample <- matrix(,nrow=3,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq","BIC"),dependant))

  # matrix excludes BIC
  insample <- matrix(,nrow=2,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq"),dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Calculate insample for endogenous
    #    construct_score_cors <- stats::cor(construct_scores)
    r_sq <- 1 - 1/solve(construct_score_cors[c(independant,dependant[i]),c(independant,dependant[i])])
    insample[1,i] <- r_sq[dependant[i],dependant[i]]
    insample[2,i] <- 1 - (1 - insample[1,i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
    # Calculate the BIC for the endogenous
    # Remove BIC for now
    #insample[3,i] <- BIC_func(r_sq[dependant[i],dependant[i]],length(independant),nrow(obsData),construct_scores[,dependant[i]])
  }
  return(insample)
}
