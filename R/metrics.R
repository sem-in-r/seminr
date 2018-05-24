# PURPOSE: Functions to compute metrics on measurement or structure

# Function to apply over manifests of a construct and return VIF values
compute_vif <- function(target, predictors, model_data) {
  independents_regr <- stats::lm(paste(target," ~."),
                                 data = as.data.frame(model_data[,predictors]))

  r_squared <- summary(independents_regr)$r.squared
  1/(1 - r_squared)
}

# Function to calculate SRMR for an implied and observed cor matrix
compute_SRMR <- function(observed, implied) {

  # calculate residuals for only upper triangle
  res <- implied - observed
  upper_res <- res[upper.tri(res, diag = TRUE)]

  # Calculate model SRMR
  ## BUT this is different from Adanco, which is different from SmarTPLS!!
  ## Henseler (2014) mentions ignoring within block residuals - is this the difference?
  sqrt(mean(upper_res^2))
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

# function to compute Henseler's rhoA
compute_construct_rhoA <- function(weights,mmMatrix,construct, obsData) {
  # get the weights for the construct
  w <- as.matrix(weights[mmMatrix[mmMatrix[,"construct"]==construct,"measurement"],construct])

  # Get empirical covariance matrix of lv indicators (S)
  indicators <- scale(obsData[,mmMatrix[mmMatrix[,"construct"]==construct,"measurement"]],TRUE,TRUE)
  S <- stats::cov(indicators,indicators)
  diag(S) <- 0

  # Get AA matrix without diagonal
  AAnondiag <- w %*% t(w)
  diag(AAnondiag) <- 0

  # Calculate rhoA
  return((t(w) %*% w)^2 * ((t(w) %*% (S) %*% w)/(t(w) %*% AAnondiag %*% w)))
}

