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

  # Sort lambda matrix (not necessary)
  #lambda_matrix <- lambda_matrix[unlist(lapply(as.list(colnames(seminr_model$path_coef)),items_of_construct, model = seminr_model)),]

  # collect the paths matrix with a 1 diagonal
  # For saturated model:
  sat_paths_matrix <- stats::cor(seminr_model$construct_scores)

  #######
  # For regular model use the total correlations matrix with a 1 diagonal:
  # create total path matrix
  total_path_matrix <- matrix(0,
                              nrow = nrow(sat_paths_matrix),
                              ncol = ncol(sat_paths_matrix),
                              dimnames = list(row.names(sat_paths_matrix),row.names(sat_paths_matrix)))
  # Collect paths matrix
  total_path_matrix[seminr_model$path_coef > 0] <- sat_paths_matrix[seminr_model$path_coef > 0]
  # make lower triangle equal upper triangle
  total_path_matrix[lower.tri(total_path_matrix)] <- t(total_path_matrix)[lower.tri(total_path_matrix)]
  # make diagonal = 1
  diag(total_path_matrix) <- 1
  #calculate indirect_paths
  holder <- total_path_matrix
  #####
  # This worked for just Image -> Expectation, Satisfaction
  #indirect_paths <- ((sat_paths_matrix - diag(1,nrow(sat_paths_matrix),nrow(sat_paths_matrix))) %*% (sat_paths_matrix - diag(1,nrow(sat_paths_matrix),nrow(sat_paths_matrix))))

  # identify all PURELY exogenous constructs
  exogenous <- setdiff(unique(seminr_model$smMatrix[,"source"]),unique(seminr_model$smMatrix[,"target"]))
  # This worked for Image -> Expectation, Satisfaction, Value
  # identify endogenous variables
  for (exo_construct in exogenous) {
    for (i in 1:nrow(seminr_model$path_coef)) {
      for (j in 1:ncol(seminr_model$path_coef)) {
        if (holder[i,j] == 0) {
          total_path_matrix[i,j] <- sat_paths_matrix[exo_construct,i] * sat_paths_matrix[exo_construct,j]
        }
      }
    }
  }
  # assign indirect paths to total effects matrix
  #total_path_matrix[total_path_matrix == 0] <- indirect_paths[total_path_matrix == 0]
  #######
  # only upper triangle
  #total_path_matrix[lower.tri(total_path_matrix,diag = FALSE)] <- 0

  #total_paths_matrix2 <- total_effects(seminr_model$path_coef) + diag(1,nrow(seminr_model$path_coef),nrow(seminr_model$path_coef))

  #total_paths_matrix <- matrix(0, nrow = nrow(sat_paths_matrix), ncol = ncol(sat_paths_matrix))
  #total_paths_matrix[path_coef > 0] <- sat_paths_matrix[seminr_model$path_coef > 0]

  #indirect_paths <- ((sat_paths_matrix - diag(1,3,3)) %*% (sat_paths_matrix - diag(1,3,3)))

  #indirect_paths[]
  # No, try the correlation model as in the sat_paths_matrix, but only with correlations where there are paths
  #new_paths_matrix <- (sat_paths_matrix * (seminr_model$path_coef > 0)) + diag(1,nrow(seminr_model$path_coef),nrow(seminr_model$path_coef))

  #seminr_model$path_coef[upper.tri(seminr_model$path_coef)] == 0
  #new_paths_matrix <- (sat_paths_matrix * (seminr_model$path_coef > 0)) + diag(1,nrow(seminr_model$path_coef),nrow(seminr_model$path_coef))
  ######

  # Calculate the covariance matrix of the item errors (only within block)
  error_cov <- error_cov_matrix(seminr_model)

  # 5 calculate model implied covariance matrix for saturated and total model
  # Rvv = Pvf.Rff.Pfv' + Dvv
  sat_imp <- lambda_matrix %*% sat_paths_matrix %*% t(lambda_matrix) + error_cov
  model_imp <- lambda_matrix %*% total_path_matrix %*% t(lambda_matrix) + error_cov

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
