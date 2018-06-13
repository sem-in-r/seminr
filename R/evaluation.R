evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  out <- list(rel,val)
  names(out) <- c("reliability","validity")
  return(out)
}

# Reliability ----
reliability <- function(seminr_model) {
  mat1 <- rhoC_AVE(seminr_model)
  mat2 <- rho_A(seminr_model)
  return(cbind(mat1,mat2))
}

# Validity ----
validity <- function(seminr_model) {
  list(
    # htmt            = HTMT(seminr_model),
    cross_loadings  = cross_loadings(seminr_model),
    item_vifs       = item_vifs(seminr_model),
    antecedent_vifs = antecedent_vifs(seminr_model)
  )
}

cross_loadings <- function(seminr_model) {
  return(stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$construct_scores))
}

# Model Fit ----


