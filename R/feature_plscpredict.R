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

# Admissibility checks ----
#
# Numerical tolerance for the PLSc admissibility checks. Every matrix checked is
# a correlation matrix (unit diagonal), so one absolute tolerance fits all of them
plsc_admissibility_tol <- 1e-8

stop_inadmissible_plsc <- function(...) {
  stop("PLSc solution is inadmissible, so the model-implied prediction is unavailable: ",
       ..., call. = FALSE)
}

is_positive_definite <- function(m) {
  min(eigen(m, symmetric = TRUE, only.values = TRUE)$values) > plsc_admissibility_tol
}

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

  disattenuated <- plsc_disattenuation(pls_model)
  rho <- disattenuated$rho[constructs]
  if (any(!is.finite(rho) | rho <= 0 | rho > 1)) {
    stop_inadmissible_plsc("rho_A outside (0, 1] (",
                           paste(sprintf("%s = %.3f", constructs, rho), collapse = ", "), ")")
  }

  reflectives <- all_factors(pls_model)
  reflective_items <- all_items_of_constructs(mmMatrix, reflectives)
  squared_loadings <- rowSums(loadings[reflective_items, , drop = FALSE]^2)
  above_one <- squared_loadings > 1 + plsc_admissibility_tol
  if (any(above_one)) {
    stop_inadmissible_plsc("standardized loading above one (",
                           paste(reflective_items[above_one], collapse = ", "), ")")
  }

  # Disattenuated construct correlations, the same as in PLSc()
  phi <- disattenuated$construct_cors[constructs, constructs]
  if (max(abs(phi[upper.tri(phi)])) >= 1) {
    stop_inadmissible_plsc("a disattenuated construct correlation is 1 or more in absolute value")
  }

  # Structural-model-implied correlations, in causal order. Only the exogenous
  # block of phi is used; correlations with endogenous constructs are implied
  # by the paths
  implied_phi <- matrix(0, length(constructs), length(constructs),
                        dimnames = list(constructs, constructs))
  earlier <- only_exogenous(smMatrix)
  implied_phi[earlier, earlier] <- phi[earlier, earlier]
  for (endogenous in construct_order(smMatrix)) {
    cov_with_earlier <- implied_phi[earlier, earlier, drop = FALSE] %*%
      pls_model$path_coef[earlier, endogenous]
    implied_phi[earlier, endogenous] <- implied_phi[endogenous, earlier] <- cov_with_earlier
    implied_phi[endogenous, endogenous] <- 1
    earlier <- c(earlier, endogenous)
  }
  if (!is_positive_definite(implied_phi)) {
    stop_inadmissible_plsc("the implied construct correlation matrix is not positive definite")
  }

  sigma <- loadings %*% implied_phi %*% t(loadings)
  observed <- stats::cor(pls_model$data[, items])
  diag(sigma) <- 1
  for (construct in all_composites(pls_model)) {
    block <- construct_items(mmMatrix, construct)
    sigma[block, block] <- observed[block, block]
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

  if (is_technique(technique, predict_DA)) {
    predictors_of <- function(construct) construct_antecedents(smMatrix, construct)
  } else if (is_technique(technique, predict_EA)) {
    exogenous <- only_exogenous(smMatrix)
    predictors_of <- function(construct) exogenous
  } else {
    stop("PLSc models can only be predicted with predict_DA or predict_EA")
  }

  predicted_items <- scaled_data[, items, drop = FALSE]
  for (construct in all_endogenous(smMatrix)) {
    x <- all_items_of_constructs(mmMatrix, predictors_of(construct))
    y <- construct_items(mmMatrix, construct)
    sigma_xx <- sigma[x, x, drop = FALSE]
    if (!is_positive_definite(sigma_xx)) {
      stop_inadmissible_plsc("the implied correlation matrix of the predictors of ", construct,
                             " is not positive definite")
    }
    predicted_items[, y] <- scaled_data[, x, drop = FALSE] %*% solve(sigma_xx, sigma[x, y, drop = FALSE])
  }

  list(items = predicted_items,
       construct_scores = predicted_items %*% weights)
}
