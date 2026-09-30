# ============================================================================
# Prediction for CB-SEM models (#426)
# ============================================================================
#
# CB-SEM models are predicted with the model-implied conditional expectation
# of de Rooij et al. (2023), E[y | x] = mu_y + Sigma_yx Sigma_xx^-1 (x - mu_x),
# using the covariance matrix implied by the lavaan fit. This is the rule that
# lavaan::lavPredictY() implements, with the predictor items chosen by the
# seminr prediction technique so that CB-SEM, PLS and PLSc predictions answer
# the same question.

# Does a CB-SEM model contain interaction terms? ----
cbsem_has_interaction <- function(model) {
  any(grepl("interaction", names(model$measurement_model))) ||
    any(grepl("_x_", model$smMatrix[, "source"], fixed = TRUE))
}

#' Predict method for SEMinR CB-SEM models
#'
#' Generates out-of-sample predictions for a model estimated by
#' \code{estimate_cbsem()}, using the model-implied conditional expectation of
#' de Rooij et al. (2023): the items of each endogenous construct are predicted
#' from the predictor items through the covariance matrix implied by the fitted
#' model. This is the rule implemented by \code{lavaan::lavPredictY()}; the
#' prediction technique chooses the predictor items.
#'
#' \itemize{
#'   \item \code{predict_DA}: each endogenous construct's items are predicted from
#'     the items of its direct antecedents.
#'   \item \code{predict_EA}: all endogenous items are predicted from the items of
#'     the exogenous constructs.
#' }
#'
#' Items of exogenous constructs are returned as observed. Prediction stops for
#' models with interaction terms (a latent product is not normally distributed,
#' so the linear rule does not apply; see
#' \url{https://github.com/sem-in-r/seminr/issues/427}), higher-order constructs,
#' ordinal indicators, multiple groups, and non-converged or inadmissible fits
#' (for example a negative residual variance).
#'
#' @param object An estimated \code{cbsem_model} from \code{estimate_cbsem()}.
#' @param testData A data.frame of held-out test data containing all indicator columns.
#' @param technique The prediction technique: \code{predict_DA} (Direct Antecedents,
#'   default) or \code{predict_EA} (Earliest Antecedents).
#' @param na.print Character string for printing NA values.
#' @param digits Number of digits for printing.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{predicted_seminr_model} object containing:
#'   \item{testData}{The test data (indicator columns).}
#'   \item{predicted_items}{Predicted indicator scores on the raw scale.}
#'   \item{item_residuals}{Residuals (actual - predicted) for each indicator.}
#'   \item{predicted_construct_scores}{Conditional expectations of the latent
#'     variables given the predictor items (for exogenous constructs, given
#'     their own items).}
#'
#' @references de Rooij, M., Karch, J. D., Fokkema, M., Bakk, Z., Pratiwi, B. C., & Kelderman, H.
#'   (2023). SEM-based out-of-sample predictions. \emph{Structural Equation Modeling}, 30(1), 132--148.
#'
#' @examples
#' mobi_mm <- constructs(
#'   reflective("Image",        multi_items("IMAG", 1:5)),
#'   reflective("Expectation",  multi_items("CUEX", 1:3)),
#'   reflective("Satisfaction", multi_items("CUSA", 1:3))
#' )
#' mobi_sm <- relationships(
#'   paths(from = "Image", to = c("Expectation", "Satisfaction")),
#'   paths(from = "Expectation", to = "Satisfaction")
#' )
#' model <- estimate_cbsem(mobi[1:200, ], mobi_mm, mobi_sm)
#' predictions <- predict(model, testData = mobi[201:250, ], technique = predict_DA)
#' head(predictions$predicted_items)
#'
#' @export
predict.cbsem_model <- function(object, testData, technique = predict_DA, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "cbsem_model"))
  fit <- object$lavaan_output
  mmMatrix <- object$mmMatrix
  smMatrix <- object$smMatrix

  unavailable <- function(...) {
    stop("Prediction is not available for this CB-SEM model: ", ..., call. = FALSE)
  }
  if (cbsem_has_interaction(object)) {
    unavailable("it contains interaction terms, and a latent product is not normally distributed ",
                "(see https://github.com/sem-in-r/seminr/issues/427).")
  }
  if (length(all_HOCs(object$measurement_model, smMatrix)) > 0) {
    unavailable("higher-order constructs are not supported.")
  }
  if (lavaan::lavInspect(fit, "ngroups") > 1) {
    unavailable("multiple-group models are not supported.")
  }
  if (length(lavaan::lavNames(fit, "ov.ord")) > 0) {
    unavailable("ordinal indicators are not supported.")
  }
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    unavailable("lavaan did not converge.")
  }
  if (!isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))) {
    unavailable("the solution is inadmissible (a negative variance or a non-positive-definite ",
                "covariance matrix; see lavaan::lavInspect(model$lavaan_output, \"post.check\")).")
  }

  if (is_technique(technique, predict_DA)) {
    predictors_of <- function(construct) construct_antecedents(smMatrix, construct)
  } else if (is_technique(technique, predict_EA)) {
    exogenous <- only_exogenous(smMatrix)
    predictors_of <- function(construct) exogenous
  } else {
    stop("CB-SEM models can only be predicted with predict_DA or predict_EA")
  }

  # Implied covariances of items and latent variables; training means of the
  # items lavaan used (the model is fitted without a mean structure, so the
  # implied means are the sample means)
  sigma <- lavaan::lavInspect(fit, "cov.all")
  items <- lavaan::lavNames(fit, "ov")
  mu <- colMeans(lavaan::lavInspect(fit, "data"))
  names(mu) <- items
  centered <- sweep(as.matrix(testData[, items, drop = FALSE]), 2, mu[items])

  conditional_expectation <- function(x, y) {
    sigma_xx <- sigma[x, x, drop = FALSE]
    eigenvalues <- eigen(sigma_xx, symmetric = TRUE, only.values = TRUE)$values
    if (min(eigenvalues) <= 1e-10 * max(eigenvalues)) {
      unavailable("the implied covariance matrix of the predictor items is not positive definite.")
    }
    centered[, x, drop = FALSE] %*% solve(sigma_xx, sigma[x, y, drop = FALSE])
  }

  constructs <- intersect(object$constructs, colnames(sigma))
  predicted_items <- as.matrix(testData[, items, drop = FALSE])
  construct_scores <- matrix(NA_real_, nrow(testData), length(constructs),
                             dimnames = list(rownames(testData), constructs))
  for (construct in constructs) {
    if (construct %in% all_endogenous(smMatrix)) {
      x <- all_items_of_constructs(mmMatrix, predictors_of(construct))
      y <- construct_items(mmMatrix, construct)
      predicted_items[, y] <- sweep(conditional_expectation(x, y), 2, mu[y], "+")
    } else {
      x <- construct_items(mmMatrix, construct)
    }
    construct_scores[, construct] <- conditional_expectation(x, construct)
  }

  predictResults <- list(
    testData = testData[, items],
    predicted_items = predicted_items,
    item_residuals = testData[, items] - predicted_items,
    predicted_construct_scores = construct_scores
  )
  class(predictResults) <- "predicted_seminr_model"
  predictResults
}
