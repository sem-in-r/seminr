# PURPOSE: Functions to compute metrics on measurement or structure

# Returns Composite Reliability given construct's loadings
compute_rhoC <- function(lambdas) {
  sum(lambdas)^2 / (sum(lambdas)^2 + sum(1-lambdas^2))
}

# Returns Average Variance Extracted given construct's loadings
compute_AVE <- function(lambdas) {
  sum(lambdas^2) / length(lambdas)
}

#' Returns R-sq of a dv given correlation matrix of ivs, dv
#'
#' @param cor_matrix A correlation matrix that includes ivs and dv
#' @param dv_name Character string of dependent variable
#' @param iv_names Vector of character strings for independent variables
#'
#' @examples
#' cors <- cbsem_summary$descriptives$correlations$constructs
#' cor_rsq(cors, dv_name = "Value", iv_names = c("Image", "Quality"))
#
cor_rsq <- function(cor_matrix, dv_name, iv_names) {
  iv_cors <- cor_matrix[iv_names, iv_names]
  dv_cors <- cor_matrix[iv_names, dv_name]
  as.numeric(t(dv_cors) %*% solve(iv_cors) %*% dv_cors)
}

# Computes Variance Inflation Factor (VIF) for named independent variables
# in a correlation table
cor_vifs <- function(cor_matrix, iv_names) {
  sapply(iv_names, function(iv) {
    rsq_j <- cor_rsq(cor_matrix, dv_name = iv, iv_names = iv_names[iv_names != iv])
    ret <- as.matrix(1/(1 - rsq_j))
    convert_to_table_output(ret)
  }, USE.NAMES = TRUE)
}

# Function to apply over manifests of a construct and return VIF values
compute_vif <- function(target, predictors, model_data) {
  independents_regr <- stats::lm(paste("`", target,"` ~.", sep = ""),
                                 data = as.data.frame(model_data[,predictors]))

  r_squared <- summary(independents_regr)$r.squared
  1/(1 - r_squared)
}


# BIC function using rsq, SST, n pk
BIC_func <- function(rsq, pk, N, construct_score){
  SSerrk <- (1-rsq)*(stats::var(construct_score)*(N-1))
  N*log(SSerrk/N) + (pk+1)*log(N)
}

# AIC function using rsq, SST, n pk
AIC_func <- function(rsq, pk, N, construct_score){
  SSerrk <- (1-rsq)*(stats::var(construct_score)*(N-1))
  2*(pk+1)+N*log(SSerrk/N)
}

return_AIC_BIC <- function(i, seminr_model) {
  antecedents <- antecedents_of(outcome = i, smMatrix = seminr_model$smMatrix)
  pk <- length(antecedents)
  rsq <- cor_rsq(stats::cor(seminr_model$construct_scores), dv_name = i, iv_names = antecedents)
  N <- nrow(seminr_model$construct_scores)
  construct_score <- seminr_model$construct_scores[,i]
  construct_AIC <- AIC_func(rsq,pk,N,construct_score)
  construct_BIC <- BIC_func(rsq,pk,N,construct_score)
  return(c(construct_AIC, construct_BIC))
}

calculate_itcriteria <- function(seminr_model) {
  endogenous <- all_endogenous(seminr_model$smMatrix)
  ret <- sapply(endogenous, return_AIC_BIC, seminr_model = seminr_model)
  rownames(ret) <- c("AIC", "BIC")
  convert_to_table_output(ret)
}

# Computes Henseler's rhoA
compute_construct_rhoA <- function(weights, mmMatrix, construct, obsData) {
  # get the weights for the construct
  w <- as.matrix(weights[mmMatrix[mmMatrix[, "construct"]==construct, "measurement"], construct])

  # Get empirical covariance matrix of lv indicators (S)
  indicators <- scale(obsData[,mmMatrix[mmMatrix[,"construct"]==construct, "measurement"]], TRUE, TRUE)
  S <- stats::cov(indicators, indicators)
  diag(S) <- 0

  # Get AA matrix without diagonal
  AAnondiag <- w %*% t(w)
  diag(AAnondiag) <- 0

  # Calculate rhoA
  return((t(w) %*% w)^2 * ((t(w) %*% (S) %*% w)/(t(w) %*% AAnondiag %*% w)))
}

#' Function to calculate Akaike weights for IT Criteria
#'
#' @param vector_of_itcriteria This argument is a vector consisting of the IT criterion estimated
#'   value for each model.
#'
#' @export
compute_itcriteria_weights <- function(vector_of_itcriteria) {
  delta_itcriteria <- vector_of_itcriteria - min(vector_of_itcriteria)
  rel_likelihoods <- exp(-0.5 * delta_itcriteria)
  sum_likelihoods <- sum(rel_likelihoods, na.rm = TRUE)
  return(rel_likelihoods / sum_likelihoods)
}
