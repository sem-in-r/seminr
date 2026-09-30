# ============================================================================
# Prediction for consistent PLS (PLSc) models (#425)
# ============================================================================
#
# PLSc path coefficients and loadings describe the common factors, not the
# weighted composites. Pushing composite scores through them (the W x B x L^T
# chain used for PLS) mixes the two metrics: with one exogenous construct the
# predictions are over-dispersed by 1/sqrt(rho_A), and with correlated
# antecedents the antecedents are reweighted. PLSc models are therefore
# predicted with the model-implied conditional expectation of de Rooij et al.
# (2023), E[y | x] = Sigma_yx Sigma_xx^-1 x on the standardized scale, where
# Sigma is the indicator correlation matrix implied by the PLSc estimates.

# Model-implied indicator correlation matrix of a PLSc model ----
#
# Reflective blocks: lambda lambda' with unit diagonal (Theta = 1 - lambda^2).
# Composite blocks: the observed within-block correlations. Between blocks:
# lambda_k phi_kl lambda_l', where for a composite the "loading" is the
# item-composite correlation (S_kk w_k). Phi is the construct correlation
# matrix implied by the structural model, starting from the correlations that
# PLSc disattenuated with the same rho_A. Inadmissible solutions stop: a
# loading above one is a negative residual variance and is not floored.
#
# @param pls_model  An estimated seminr_model with reflective constructs
# @return Named correlation matrix over the model's (non-interaction) items
plsc_implied_correlations <- function(pls_model) {
  mmMatrix <- pls_model$mmMatrix
  smMatrix <- pls_model$smMatrix
  constructs <- pls_model$constructs
  items <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  loadings <- pls_model$outer_loadings[items, constructs, drop = FALSE]
  inadmissible <- function(...) {
    stop("PLSc solution is inadmissible, so the model-implied prediction is unavailable: ",
         ..., call. = FALSE)
  }

  rho <- rho_A(pls_model, constructs)[, 1]
  if (any(!is.finite(rho) | rho <= 0 | rho > 1)) {
    inadmissible("rho_A outside (0, 1] (",
                 paste(sprintf("%s = %.3f", constructs, rho), collapse = ", "), ")")
  }

  reflectives <- intersect(all_reflective(mmMatrix), constructs)
  reflective_items <- unlist(lapply(reflectives, function(x) construct_items(mmMatrix, x)))
  squared_loadings <- rowSums(loadings[reflective_items, , drop = FALSE]^2)
  if (any(squared_loadings > 1 + 1e-8)) {
    inadmissible("standardized loading above one (",
                 paste(reflective_items[squared_loadings > 1 + 1e-8], collapse = ", "), ")")
  }

  # Disattenuated construct correlations, as in PLSc()
  phi <- stats::cor(pls_model$construct_scores[, constructs]) / sqrt(outer(rho, rho))
  diag(phi) <- 1
  if (max(abs(phi[upper.tri(phi)])) >= 1) {
    inadmissible("a disattenuated construct correlation is 1 or more in absolute value")
  }

  # Structural-model-implied correlations, in causal order
  exogenous <- only_exogenous(smMatrix)
  ordered <- c(exogenous, construct_order(smMatrix))
  implied_phi <- matrix(0, length(constructs), length(constructs),
                        dimnames = list(constructs, constructs))
  implied_phi[exogenous, exogenous] <- phi[exogenous, exogenous]
  for (i in seq_along(ordered)[-seq_along(exogenous)]) {
    endogenous <- ordered[i]
    earlier <- ordered[seq_len(i - 1)]
    cov_with_earlier <- implied_phi[earlier, earlier, drop = FALSE] %*%
      pls_model$path_coef[earlier, endogenous]
    implied_phi[earlier, endogenous] <- implied_phi[endogenous, earlier] <- cov_with_earlier
    implied_phi[endogenous, endogenous] <- 1
  }
  if (min(eigen(implied_phi, symmetric = TRUE, only.values = TRUE)$values) <= 1e-8) {
    inadmissible("the implied construct correlation matrix is not positive definite")
  }

  sigma <- loadings %*% implied_phi %*% t(loadings)
  observed <- stats::cor(pls_model$data[, items])
  for (construct in constructs) {
    block <- construct_items(mmMatrix, construct)
    if (construct %in% reflectives) {
      sigma[cbind(block, block)] <- 1
    } else {
      sigma[block, block] <- observed[block, block]
    }
  }
  sigma
}

# Model-implied predictions for a PLSc model ----
#
# DA predicts each endogenous construct's items from the items of its direct
# antecedents; EA predicts them from the items of the exogenous constructs.
# Exogenous items are returned as observed. Construct predictions are the
# expected composite scores E[z_block | x] w, i.e. the metric of the construct
# scores they are compared with (actual_star).
#
# @param pls_model    An estimated seminr_model with reflective constructs
# @param scaled_data  Test items standardized with the model's training moments
# @param technique    predict_DA or predict_EA
# @return list(items = standardized item predictions, construct_scores = ...)
plsc_implied_predictions <- function(pls_model, scaled_data, technique) {
  mmMatrix <- pls_model$mmMatrix
  smMatrix <- pls_model$smMatrix
  sigma <- plsc_implied_correlations(pls_model)
  items <- colnames(sigma)
  weights <- pls_model$outer_weights[items, pls_model$constructs, drop = FALSE]

  if (identical(technique, predict_DA)) {
    predictors_of <- function(construct) construct_antecedents(smMatrix, construct)
  } else if (identical(technique, predict_EA)) {
    exogenous <- only_exogenous(smMatrix)
    predictors_of <- function(construct) exogenous
  } else {
    stop("PLSc models can only be predicted with predict_DA or predict_EA")
  }

  predicted_items <- scaled_data[, items, drop = FALSE]
  for (construct in all_endogenous(smMatrix)) {
    x <- unlist(lapply(predictors_of(construct), function(p) construct_items(mmMatrix, p)))
    y <- construct_items(mmMatrix, construct)
    sigma_xx <- sigma[x, x, drop = FALSE]
    eigenvalues <- eigen(sigma_xx, symmetric = TRUE, only.values = TRUE)$values
    if (min(eigenvalues) <= 1e-10 * max(eigenvalues)) {
      stop("PLSc solution is inadmissible, so the model-implied prediction is unavailable: ",
           "the implied correlation matrix of the predictors of ", construct,
           " is not positive definite", call. = FALSE)
    }
    predicted_items[, y] <- scaled_data[, x, drop = FALSE] %*% solve(sigma_xx, sigma[x, y, drop = FALSE])
  }

  list(items = predicted_items,
       construct_scores = predicted_items %*% weights)
}
