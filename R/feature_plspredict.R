# ============================================================================
# Shared prediction helpers
# ============================================================================

# Legacy helper used by predict_pls for construct-level actuals ----
estimate_actual_star <- function(pls_model, train_data, testData) {
  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  actual_star <- estimate_pls(data = rbind(train_data[,no_int_mmvars], testData[,no_int_mmvars]),
                                       measurement_model = pls_model$measurement_model,
                                       structural_model = pls_model$structural_model)$construct_scores[,all_endogenous(pls_model$smMatrix),drop = F] |> suppressMessages()

  actual_star_out <- actual_star[(nrow(train_data)+1):(nrow(testData) + nrow(train_data)),,drop = F]
  actual_star_in <- actual_star[1:(nrow(train_data)),,drop = F]
  return(list(actual_oos = actual_star_out,
              actual_is = actual_star_in))
}

# Parse interaction name "X*Y" into component construct names ----
parse_interactions <- function(x) {
  ind1 <- regexpr("\\*", x)
  return(list(interaction = x,
              antecedent = substr(x,0,ind1-1),
              moderator = substr(x, ind1+1,nchar(x)),
              inter_ind = paste(x,"_intxn",sep = "")))
}

# Compute two-stage moderator scores from construct score products ----
return_mod_scores <- function(OOS_composite_scores, testData, x) {
  Moderator_score <- matrix(OOS_composite_scores[, x$antecedent] * OOS_composite_scores[,x$moderator], ncol = 1)
  colnames(Moderator_score) <- x$inter_ind
  return(Moderator_score)
}

# Re-estimate PLS on combined train+test data to get reference construct scores ----
#
# Used by all predict functions. Replaces test rows in the training data, then
# re-estimates the full model (including interactions via process_interactions).
# Returns the full construct_scores matrix for all observations.
#
# @param pls_model  The trained seminr_model
# @param testData   Held-out test data (data.frame)
# @return construct_scores matrix from the re-estimated model
compute_actual_star <- function(pls_model, testData) {
  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  fulldata <- pls_model$data[, no_int_mmvars]
  fulldata[rownames(testData), no_int_mmvars] <- testData[, no_int_mmvars]
  suppressMessages(
    fullmodel <- estimate_pls(
      data = fulldata,
      measurement_model = pls_model$measurement_model,
      structural_model = pls_model$structural_model,
      inner_weights = pls_model$inner_weights,
      missing = pls_model$settings$missing,
      missing_value = pls_model$settings$missing_value,
      maxIt = pls_model$settings$maxIt,
      stopCriterion = pls_model$settings$stopCriterion
    )
  )
  fullmodel$construct_scores
}

# Core prediction pipeline: standardize → W×B×L^T → unstandardize → return ----
#
# Shared by all interaction predict functions (two_stage, product_indicator,
# orthogonal). Takes augmented test data (raw items + interaction items already
# appended), runs the W × B × L^T prediction chain, computes residuals, and
# returns a predicted_seminr_model object.
#
# @param pls_model    The trained seminr_model
# @param testData     Original held-out test data (for residual computation)
# @param augmented_data  Test data with interaction columns appended
# @param actual_star  Construct scores from compute_actual_star()
# @param technique    predict_DA or predict_EA
# @return A predicted_seminr_model object
predict_from_augmented_data <- function(pls_model, testData, augmented_data,
                                        actual_star, technique) {
  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]

  # Standardize all items (raw + interaction) using model's stored params
  scaled_data <- standardize_data(
    as.matrix(augmented_data[, pls_model$mmVariables]),
    pls_model$meanData[pls_model$mmVariables],
    pls_model$sdData[pls_model$mmVariables]
  )

  # W × B × L^T prediction chain
  predicted_construct_scores <- scaled_data %*% pls_model$outer_weights[pls_model$mmVariables, ]
  predicted_construct_scores <- technique(pls_model$smMatrix, pls_model$path_coef,
                                          predicted_construct_scores)
  predictedMeasurements <- predicted_construct_scores %*% t(pls_model$outer_loadings)

  # Unstandardize non-interaction items only
  predictedMeasurements <- unstandardize_data(
    predictedMeasurements[, no_int_mmvars],
    pls_model$meanData[no_int_mmvars],
    pls_model$sdData[no_int_mmvars]
  )
  colnames(predictedMeasurements) <- no_int_mmvars

  residuals <- testData[, no_int_mmvars] - predictedMeasurements[, no_int_mmvars]

  predictResults <- list(
    testData = testData[, no_int_mmvars],
    predicted_items = predictedMeasurements[, no_int_mmvars],
    item_residuals = residuals[, no_int_mmvars],
    predicted_composite_scores = predicted_construct_scores,
    composite_residuals = (actual_star[rownames(testData), ] - predicted_construct_scores),
    actual_star = actual_star[rownames(testData), ]
  )
  class(predictResults) <- "predicted_seminr_model"
  predictResults
}

# ============================================================================
# Predict functions for each model type
# ============================================================================

# Prediction for models without interactions ----
one_stage_predict <- function(pls_model, testData, technique) {
  actual_star <- compute_actual_star(pls_model, testData)
  predict_from_augmented_data(pls_model, testData, testData, actual_star, technique)
}

# Prediction for two_stage interaction models ----
#
# Two-stage prediction uses construct-score products rather than item-level
# products. It re-estimates a first-stage model (without interactions) to get
# OOS composite scores, then multiplies IV * Moderator scores to create the
# interaction indicator.
two_stage_predict <- function(pls_model, testData, technique) {
  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  actual_star <- compute_actual_star(pls_model, testData)

  # Collect all interactions and parse their IV/moderator names
  interactions <- pls_model$constructs[is_interaction(pls_model$constructs)]
  int_list <- lapply(interactions, parse_interactions)

  # Re-estimate first-stage model (without interactions) on training data
  first_stage_mm <- pls_model$measurement_model[!(all_constructs(pls_model$mmMatrix) %in% interactions)]
  first_stage_sm <- remove_paths_from(pls_model$structural_model, interactions)
  first_stage_model <- estimate_pls(data = pls_model$rawdata,
                                    measurement_model = first_stage_mm,
                                    structural_model = first_stage_sm) |> suppressMessages()

  # Compute OOS composite scores using first-stage weights
  scaled_data <- standardize_data(testData[, no_int_mmvars, drop = FALSE],
                                  first_stage_model$meanData[no_int_mmvars],
                                  first_stage_model$sdData[no_int_mmvars])
  OOS_composite_scores <- as.matrix(scaled_data) %*%
    first_stage_model$outer_weights[no_int_mmvars, , drop = FALSE]

  # Create moderator scores (IV * Moderator construct scores)
  mod_scores <- lapply(int_list, function(x) {
    return_mod_scores(OOS_composite_scores, testData, x)
  })
  augmented_data <- cbind(testData, do.call("cbind", mod_scores))

  predict_from_augmented_data(pls_model, testData, augmented_data, actual_star, technique)
}


# Recreate product indicator items for held-out test data ----
#
# During estimation, product_indicator() and orthogonal() in specify_interactions.R
# create interaction items by:
#   1. Standardizing IV and moderator items using scale() on the training data
#   2. Computing all pairwise products of the standardized items
#
# At prediction time, test data does NOT contain interaction columns. This function
# recreates them using the TRAINING data's scaling parameters (mean, sd) so that
# the test-data products are on the same scale as the training-data products.
#
# IMPORTANT: We use rawdata (original data without interaction columns) to compute
# the base-item means/SDs. This is NOT the same as model$meanData, which includes
# the product items themselves and uses a different standardization (the full-data
# standardization applied after interaction processing in simplePLS).
#
# @param pls_model  An estimated seminr_model (contains rawdata, mmMatrix)
# @param testData   Held-out test data (data.frame with raw indicator columns)
# @param interaction_name  Interaction construct name, e.g. "Image*Expectation"
# @return A data.frame of product indicator columns with names matching estimation
#         (e.g. "IMAG1*CUEX1", "IMAG1*CUEX2", ...). Same row count as testData.
create_pi_items_for_test_data <- function(pls_model, testData, interaction_name) {
  # Parse "X*Y" to extract IV and moderator construct names
  parsed <- parse_interactions(interaction_name)
  iv_name <- parsed$antecedent
  mod_name <- parsed$moderator

  # Look up indicator names for each construct from the measurement model matrix
  iv_items <- construct_items(pls_model$mmMatrix, iv_name)
  mod_items <- construct_items(pls_model$mmMatrix, mod_name)

  # Compute training-data scaling parameters from rawdata (base items only).
  # This mirrors what scale() does inside product_indicator()/orthogonal() at
  # estimation time (specify_interactions.R lines 237-238 and 151-152).
  train_iv <- pls_model$rawdata[, iv_items, drop = FALSE]
  train_mod <- pls_model$rawdata[, mod_items, drop = FALSE]
  iv_center <- colMeans(train_iv, na.rm = TRUE)
  iv_scale <- apply(train_iv, 2, stats::sd, na.rm = TRUE)
  mod_center <- colMeans(train_mod, na.rm = TRUE)
  mod_scale <- apply(train_mod, 2, stats::sd, na.rm = TRUE)

  # Standardize test data items using training params (NOT test-data params)
  scaled_iv <- as.data.frame(standardize_data(
    as.matrix(testData[, iv_items, drop = FALSE]), iv_center, iv_scale))
  scaled_mod <- as.data.frame(standardize_data(
    as.matrix(testData[, mod_items, drop = FALSE]), mod_center, mod_scale))

  # Compute all pairwise products of scaled items.
  # For p IV items and q moderator items, this creates p*q product columns.
  # Uses mult() and name_items() from library.R — same functions used at estimation.
  multiples_list <- lapply(scaled_iv, mult, scaled_mod)
  pi_data <- do.call("cbind", multiples_list)
  colnames(pi_data) <- as.vector(sapply(iv_items, name_items, mod_items))

  pi_data
}

# Prediction for product_indicator interaction models ----
#
# Recreates item-level product indicators from test data (via
# create_pi_items_for_test_data), augments the test data, then delegates
# to predict_from_augmented_data for the shared W×B×L^T pipeline.
#
# Supports multiple product_indicator interactions and quadratic terms.
product_indicator_predict <- function(pls_model, testData, technique) {
  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  actual_star <- compute_actual_star(pls_model, testData)

  # Recreate product indicator items for each interaction construct,
  # scaled using training-data params (not test-data params).
  interactions <- pls_model$constructs[is_interaction(pls_model$constructs)]
  pi_cols_list <- lapply(interactions, function(int_name) {
    create_pi_items_for_test_data(pls_model, testData, int_name)
  })
  augmented_data <- cbind(testData[, no_int_mmvars, drop = FALSE],
                          do.call("cbind", pi_cols_list))

  predict_from_augmented_data(pls_model, testData, augmented_data, actual_star, technique)
}

# Prediction for orthogonal interaction models ----
#
# Same as product_indicator_predict, but after creating raw product items,
# orthogonalizes them using regression coefficients stored during estimation.
#
# The orthogonal method (Henseler & Chin, 2010) regresses each product item
# on the ORIGINAL (unscaled) main-effect items and uses the residuals. At
# prediction time, we apply the stored coefficients: residual = product - X*beta.
#
# IMPORTANT: The X matrix uses ORIGINAL (unscaled) test data items, matching the
# lm(data = data) call in orthogonal() which uses unscaled training data as
# predictors while the dependent variable is the product of SCALED items.
orthogonal_predict <- function(pls_model, testData, technique) {
  if (is.null(pls_model$interaction_params)) {
    stop("This model was estimated with an older version of seminr that did not store ",
         "orthogonalization parameters. Please re-estimate the model with the current version.")
  }

  no_int_mmvars <- pls_model$mmVariables[!is_interaction(pls_model$mmVariables)]
  actual_star <- compute_actual_star(pls_model, testData)

  # Recreate product items, then orthogonalize using stored coefficients
  interactions <- pls_model$constructs[is_interaction(pls_model$constructs)]
  ortho_cols_list <- lapply(interactions, function(int_name) {
    # Create raw (non-orthogonalized) product items from scaled test data
    products <- create_pi_items_for_test_data(pls_model, testData, int_name)

    # Orthogonalize: subtract predicted values using stored lm() coefficients.
    # X matrix uses ORIGINAL (unscaled) test items + intercept column.
    ortho_coefs <- pls_model$interaction_params[[int_name]]$ortho_coefs
    parsed <- parse_interactions(int_name)
    iv_items <- construct_items(pls_model$mmMatrix, parsed$antecedent)
    mod_items <- construct_items(pls_model$mmMatrix, parsed$moderator)
    X_test <- as.matrix(cbind(1, testData[, c(iv_items, mod_items), drop = FALSE]))

    for (i in 1:ncol(products)) {
      products[, i] <- products[, i] - X_test %*% ortho_coefs[[i]]
    }
    products
  })
  augmented_data <- cbind(testData[, no_int_mmvars, drop = FALSE],
                          do.call("cbind", ortho_cols_list))

  predict_from_augmented_data(pls_model, testData, augmented_data, actual_star, technique)
}

# Identify which interaction estimation method was used ----
#
# Inspects the class attributes of interaction elements in the measurement model
# to determine which method was used. Each interaction_term() in constructs()
# carries a class tag set during specification:
#   - "two_stage_interaction"   → two_stage()    in specify_interactions.R
#   - "scaled_interaction"      → product_indicator() in specify_interactions.R
#   - "orthogonal_interaction"  → orthogonal()   in specify_interactions.R
#
# @param model  An estimated seminr_model
# @return A named character vector of methods, one per interaction construct.
#         Names are the interaction element names from the measurement model list.
detect_interaction_method <- function(model) {
  int_elements <- model$measurement_model[grepl("interaction", names(model$measurement_model))]
  sapply(int_elements, function(el) {
    cls <- class(el)
    if ("two_stage_interaction" %in% cls) return("two_stage")
    if ("scaled_interaction" %in% cls)    return("product_indicator")
    if ("orthogonal_interaction" %in% cls) return("orthogonal")
    return("unknown")
  })
}

# S3 predict method for SEMinR PLS models ----
#
# Dispatches to the appropriate prediction function based on model type:
#   - No interactions    → one_stage_predict()
#   - two_stage          → two_stage_predict()
#   - product_indicator  → product_indicator_predict()
#   - orthogonal         → orthogonal_predict()
#
# HOC (higher-order construct) models are not supported for prediction.
# Mixed interaction methods (e.g., one two_stage + one product_indicator) are
# not supported — all interactions in a model must use the same method.
#
#' Predict method for SEMinR PLS models
#'
#' Generates out-of-sample predictions for a PLS model estimated by \code{estimate_pls()}.
#' Supports models with and without interaction terms. For interaction models, the
#' prediction method is automatically detected from the measurement model specification:
#'
#' \itemize{
#'   \item \code{two_stage}: Recreates interaction from construct-score products
#'   \item \code{product_indicator}: Recreates scaled item-level products from test data
#'   \item \code{orthogonal}: Recreates scaled products and applies stored orthogonalization
#'     coefficients from estimation
#' }
#'
#' Higher-order construct (HOC) models are not currently supported for prediction.
#' Models with mixed interaction methods (e.g., one \code{two_stage} and one
#' \code{product_indicator}) will produce an error.
#'
#' @param object An estimated \code{seminr_model} from \code{estimate_pls()}.
#' @param testData A data.frame of held-out test data containing all indicator columns.
#'   Must not include interaction columns (these are recreated internally).
#' @param technique The prediction technique: \code{predict_DA} (Direct Antecedents,
#'   default) or \code{predict_EA} (Earliest Antecedents).
#' @param na.print Character string for printing NA values.
#' @param digits Number of digits for printing.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{predicted_seminr_model} object containing:
#'   \item{testData}{The test data (non-interaction items only).}
#'   \item{predicted_items}{Predicted indicator scores.}
#'   \item{item_residuals}{Residuals (actual - predicted) for each indicator.}
#'   \item{predicted_composite_scores}{Predicted construct scores.}
#'   \item{composite_residuals}{Residuals for construct scores.}
#'   \item{actual_star}{Reference construct scores from re-estimation on combined data.}
#'
#' @examples
#' data(mobi)
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3)),
#'   interaction_term(iv = "Image", moderator = "Expectation",
#'                    method = product_indicator)
#' )
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Image*Expectation"))
#' )
#' model <- estimate_pls(mobi, mobi_mm, mobi_sm)
#' predictions <- predict(model, testData = mobi[1:20, ])
#'
#' @export
predict.seminr_model <- function(object, testData, technique = predict_DA, na.print=".", digits=3, ...){
  stopifnot(inherits(object, "seminr_model"))

  # HOC prediction is an unsolved problem in the literature
  if (!is.null(object$hoc)) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }

  # No interactions: standard single-stage prediction
  if (is.null(object$interaction)) {
    return(one_stage_predict(object, testData, technique))
  }

  # Dispatch based on interaction method
  methods <- unique(detect_interaction_method(object))
  if (length(methods) > 1) {
    stop("Mixed interaction methods (", paste(methods, collapse = ", "),
         ") are not supported for prediction. Use a single method for all interactions.")
  }

  switch(methods,
    "two_stage"         = two_stage_predict(object, testData, technique),
    "product_indicator" = product_indicator_predict(object, testData, technique),
    "orthogonal"        = orthogonal_predict(object, testData, technique),
    stop("Unknown interaction method: ", methods)
  )
}

#' Predict_pls performs either k-fold or LOOCV on a SEMinR PLS model and generates predictions
#'
#' \code{predict_pls} uses cross-validation to generate in-sample and out-sample predictions for PLS models generated by SEMinR.
#'
#' This function generates cross-validated in-sample and out-sample predictions for PLS models generated by SEMinR. The
#' cross validation technique can be k-fold if a number of folds are specified, or leave-one-out-cross-validation (LOOCV) if no folds
#' arew specified. LOOCV is recommended for small datasets.
#'
#' @param model A SEMinR model that has been estimated on the FULL dataset.
#'
#' @param technique The predictive technique to be employed, Earliest Antecedents (EA) \code{predict_EA} or
#' Direct Antecedents (DA) \code{predict_DA}
#'
#' @param noFolds The required number of folds to use in k-fold cross validation. If NULL, then parallel LOOCV will be executed.
#' Default is NULL.
#'
#' @param reps The number of times the cross-validation will be repeated. Default is NULL.
#'
#' @param cores The number of cores to use for parallel processing. If NULL (default),
#' cross-validation runs sequentially. Specify an integer to enable parallel execution —
#' useful for LOOCV or high-k folds (e.g., \code{noFolds = 50}) where each fold requires
#' re-estimation. Note: parallel workers load the \emph{installed} version of seminr via
#' \code{library(seminr)}, so run \code{devtools::install()} if testing development code.
#'
#' @return A list of the estimated PLS and LM prediction results:
#'  \item{PLS_out_of_sample}{A matrix of the out-of-sample indicator predictions generated by the SEMinR model.}
#'  \item{PLS_in_sample}{A matrix of the in-sample indicator predictions generated by the SEMinR model.}
#'  \item{lm_out_of_sample}{A matrix of the out-of-sample indicator predictions generated by a linear regression model.}
#'  \item{lm_in_sample}{A matrix of the in-sample indicator predictions generated by a linear regression model.}
#'  \item{item_actuals}{A matrix of the actual indicator scores.}
#'  \item{PLS_out_of_sample_residuals}{A matrix of the out-of-sample indicator PLS prediction residuals.}
#'  \item{PLS_in_sample_residuals}{A matrix of the in-sample indicator PLS prediction residuals.}
#'  \item{lm_out_of_sample_residuals}{A matrix of the out-of-sample LM indicator prediction residuals.}
#'  \item{lm_in_sample_residuals}{A matrix of the in-sample LM indicator prediction residuals.}
#'  \item{mmMatrix}{A Matrix of the measurement model relations.}
#'  \item{smMatrix}{A Matrix of the structural model relations.}
#'  \item{constructs}{A vector of the construct names.}
#'  \item{mmVariables}{A vector of the indicator names.}
#'  \item{outer_loadings}{The matrix of estimated indicator loadings.}
#'  \item{outer_weights}{The matrix of estimated indicator weights.}
#'  \item{path_coef}{The matrix of estimated structural model relationships.}
#'  \item{iterations}{A numeric indicating the number of iterations required before the algorithm converged.}
#'  \item{weightDiff}{A numeric indicating the minimum weight difference between iterations of the algorithm.}
#'  \item{construct_scores}{A matrix of the estimated construct scores for the PLS model.}
#'  \item{rSquared}{A matrix of the estimated R Squared for each construct.}
#'  \item{inner_weights}{The inner weight estimation function.}
#'  \item{data}{A matrix of the data upon which the model was estimated (INcluding interactions.}
#'  \item{rawdata}{A matrix of the data upon which the model was estimated (EXcluding interactions.}
#'  \item{measurement_model}{The SEMinR measurement model specification.}
#'
#' @usage
#'
#' predict_pls(model, technique, noFolds, reps, cores)
#'
#' @examples
#' data(mobi)
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3))
#' )
#'
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value"))
#' )
#'
#' mobi_pls <- estimate_pls(mobi, mobi_mm, mobi_sm)
#' cross_validated_predictions <- predict_pls(model = mobi_pls,
#'                                            technique = predict_DA,
#'                                            noFolds = 10,
#'                                            cores = NULL)
#'
#' @export
predict_pls <- function(model, technique = predict_DA, noFolds = NULL, reps = NULL, cores = NULL) {

  stopifnot(inherits(model, "seminr_model"))
  # Abort if received a higher-order-model or moderated model
  if (!is.null(model$hoc)) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }
  # Get endogenous item names
  endogenous_items <- c(unlist(sapply(all_endogenous(model$smMatrix), function(x) construct_items(model$mmMatrix, x)), use.names = FALSE))

  # shuffle data
  order <- sample(nrow(model$data),nrow(model$data), replace = FALSE)
  ordered_data <- model$data[order,]

  # collect in-sample and out-sample prediction matrices and sort everything to original row indexes
  if(is.null(reps)) {

    pred_matrices <- prediction_matrices( noFolds, ordered_data, model,technique, cores)
    PLS_predicted_outsample_item <- pred_matrices$out_of_sample_item[rownames(model$data),]
    PLS_predicted_insample_item <- pred_matrices$in_sample_item[rownames(model$data),]
    LM_predicted_outsample_item <- pred_matrices$out_of_sample_lm_item[rownames(model$data),]
    LM_predicted_insample_item <- pred_matrices$in_sample_lm_item[rownames(model$data),]
  } else {
    no_int_mmvars <- model$mmVariables[!is_interaction(model$mmVariables)]
    pls_pred_oos_array <- array(,dim = c(nrow(ordered_data), length(no_int_mmvars), reps))
    pls_pred_is_array <- array(,dim = c(nrow(ordered_data), length(no_int_mmvars), reps))
    lm_pred_oos_array <- array(,dim = c(nrow(ordered_data), length(endogenous_items), reps))
    lm_pred_is_array <- array(,dim = c(nrow(ordered_data), length(endogenous_items), reps))
    for (i in 1:reps) {
      pred_matrices <- prediction_matrices( noFolds, ordered_data, model,technique, cores)
      pls_pred_oos_array[,,i] <- pred_matrices$out_of_sample_item[rownames(model$data),]
      pls_pred_is_array[,,i] <- pred_matrices$in_sample_item[rownames(model$data),]
      lm_pred_oos_array[,,i] <- pred_matrices$out_of_sample_lm_item[rownames(model$data),]
      lm_pred_is_array[,,i] <- pred_matrices$in_sample_lm_item[rownames(model$data),]
    }
    PLS_predicted_outsample_item <- apply(pls_pred_oos_array,c(1,2),mean)
    PLS_predicted_insample_item <- apply(pls_pred_is_array,c(1,2),mean)
    LM_predicted_outsample_item <- apply(lm_pred_oos_array,c(1,2),mean)
    LM_predicted_insample_item <- apply(lm_pred_is_array,c(1,2),mean)
    colnames(PLS_predicted_outsample_item) <- no_int_mmvars
    colnames(PLS_predicted_insample_item) <- no_int_mmvars
    colnames(LM_predicted_outsample_item) <- endogenous_items
    colnames(LM_predicted_insample_item) <- endogenous_items
  }

  # Allocate results
  results <- list(
    composites = list(
      composite_out_of_sample = pred_matrices$out_of_sample_construct[rownames(model$data),],
      composite_in_sample = pred_matrices$in_sample_construct[rownames(model$data),],
      actuals_star = model$construct_scores[rownames(model$data),]),
    items = list(PLS_out_of_sample = PLS_predicted_outsample_item[,endogenous_items],
                  PLS_in_sample = PLS_predicted_insample_item[,endogenous_items],
                  lm_out_of_sample = LM_predicted_outsample_item,
                  lm_in_sample = LM_predicted_insample_item,
                  item_actuals = ordered_data[rownames(model$data),model$mmVariables],
                  PLS_out_of_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - PLS_predicted_outsample_item[,endogenous_items]),
                  PLS_in_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - PLS_predicted_insample_item[,endogenous_items]),
                  lm_out_of_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - LM_predicted_outsample_item),
                  lm_in_sample_residuals = (ordered_data[rownames(model$data),endogenous_items] - LM_predicted_insample_item)),
    model = model)
  class(results) <- "predict_pls_model"
  return(results)
}

# Function to calculate item metrics
item_metrics <- function(pls_prediction_kfold) {

  # Genereate IS PLS metrics
  PLS_item_prediction_metrics_IS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$PLS_in_sample_residuals, 2, prediction_metrics))

  # Generate OOS PLS metrics
  PLS_item_prediction_metrics_OOS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$PLS_out_of_sample_residuals, 2, prediction_metrics))

  # Generate IS LM metrics
  LM_item_prediction_metrics_IS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$lm_in_sample_residuals, 2, prediction_metrics))

  # Generate OOS LM metrics
  LM_item_prediction_metrics_OOS <- convert_to_table_output(
    apply(pls_prediction_kfold$items$lm_out_of_sample_residuals, 2, prediction_metrics))

  # Assign rownames to matrices
  rownames(PLS_item_prediction_metrics_IS) <- rownames(PLS_item_prediction_metrics_OOS) <- rownames(LM_item_prediction_metrics_OOS) <- c("RMSE","MAE")
  rownames(LM_item_prediction_metrics_OOS) <- rownames(LM_item_prediction_metrics_IS) <- c("RMSE","MAE")

  return(list(PLS_item_prediction_metrics_IS = PLS_item_prediction_metrics_IS,
              PLS_item_prediction_metrics_OOS = PLS_item_prediction_metrics_OOS,
              LM_item_prediction_metrics_IS = LM_item_prediction_metrics_IS,
              LM_item_prediction_metrics_OOS = LM_item_prediction_metrics_OOS))
}


# Function to standardize a matrix by sd vector and mean vector
standardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(t(t(sweep(data_matrix,2,means_vector)) / sd_vector))
}

# Function to un-standardize a matrix by sd vector and mean vector
unstandardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(sweep((data_matrix %*% diag(sd_vector)),2,means_vector,"+"))
}

#$ Function to sum rows of a matrix
sum_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x]))
}

#$ Function to mean rows of a matrix
mean_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x])/(noFolds-1))
}

#### Check ----

# Function to return train and test predictions for a model
in_and_out_sample_predictions <- function(x, folds, ordered_data, model,technique) {
  testIndexes <- which(folds==x,arr.ind=TRUE)
  trainIndexes <- which(folds!=x,arr.ind=TRUE)
  testingData <- ordered_data[testIndexes, ]
  trainingData <- ordered_data[-testIndexes, ]
  no_int_mmvars <- model$mmVariables[!is_interaction(model$mmVariables)]

  # Create matrices for return data
  PLS_predicted_outsample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_insample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_outsample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  PLS_predicted_insample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  PLS_predicted_insample_item_residuals <- matrix(0,nrow = nrow(ordered_data),ncol = length(no_int_mmvars),dimnames = list(rownames(ordered_data),no_int_mmvars))
  #PLS prediction on testset model
  suppressMessages(
    train_model <- estimate_pls(
      data = trainingData,
      measurement_model = model$measurement_model,
      structural_model = model$smMatrix,
      inner_weights = model$inner_weights,
      missing = model$settings$missing,
      missing_value = model$settings$missing_value,
      maxIt = model$settings$maxIt,
      stopCriterion = model$settings$stopCriterion
    )
  )
  test_predictions <- stats::predict(object = train_model,
                                     testData = testingData,
                                     technique = technique)

  PLS_predicted_outsample_construct[testIndexes,] <-  test_predictions$predicted_composite_scores
  PLS_predicted_outsample_item[testIndexes,] <- test_predictions$predicted_items


  #PLS prediction on trainset model
  train_predictions <- stats::predict(object = train_model,
                                      testData = trainingData,
                                      technique = technique)

  PLS_predicted_insample_construct[trainIndexes,] <- train_predictions$predicted_composite_scores
  PLS_predicted_insample_item[trainIndexes,] <- train_predictions$predicted_items
  PLS_predicted_insample_item_residuals[trainIndexes,] <- as.matrix(train_predictions$item_residuals)

  ## Perform prediction on LM models for benchmark
  # Identify endogenous items
  endogenous_items <- unlist(sapply(all_endogenous(model$smMatrix), function(x) construct_items(model$mmMatrix, x)), use.names = FALSE)

  #LM Matrices
  lm_holder <- sapply(all_endogenous(model$smMatrix), generate_lm_predictions, model = model,
                      ordered_data = ordered_data[,model$mmVariables],
                      testIndexes = testIndexes,
                      endogenous_items = endogenous_items,
                      trainIndexes = trainIndexes,
                      technique = technique)

  lmprediction_in_sample <- matrix(0, ncol = 0 , nrow = length(trainIndexes))
  lmprediction_out_sample <- matrix(0, ncol = 0 , nrow = length(testIndexes))
  lmprediction_in_sample_residuals <- matrix(0,nrow=nrow(ordered_data),ncol=length(endogenous_items),byrow =TRUE,dimnames = list(rownames(ordered_data),endogenous_items))

  # collect the odd and even numbered matrices from the matrices return object
  n_endogenous <- length(all_endogenous(model$smMatrix))
  lmprediction_in_sample <- do.call(cbind, lm_holder[((1:(n_endogenous*2))[1:(n_endogenous*2)%%2==1])])
  lmprediction_out_sample <- do.call(cbind, lm_holder[((1:(n_endogenous*2))[1:(n_endogenous*2)%%2==0])])
  lmprediction_in_sample_residuals[trainIndexes,] <- as.matrix(ordered_data[trainIndexes,as.vector(endogenous_items)]) - lmprediction_in_sample[trainIndexes,as.vector(endogenous_items)]

  return(list(PLS_predicted_insample = PLS_predicted_insample_construct,
              PLS_predicted_outsample = PLS_predicted_outsample_construct,
              PLS_predicted_insample_item = PLS_predicted_insample_item,
              PLS_predicted_outsample_item = PLS_predicted_outsample_item,
              LM_predicted_insample_item = lmprediction_in_sample,
              LM_predicted_outsample_item = lmprediction_out_sample,
              PLS_predicted_insample_item_residuals = PLS_predicted_insample_item_residuals,
              LM_predicted_insample_item_residuals = lmprediction_in_sample_residuals))
}

# Collect and parse prediction matrices across k folds ----
#
# Parallelization strategy:
#   - cores specified: parallel (useful for LOOCV or high-k like 50, 100)
#   - cores = NULL (default): always sequential
#
# NOTE: Parallel workers load the INSTALLED package via library(seminr), not
# devtools::load_all(). If tests fail with "number of items to replace is not
# a multiple of replacement length" or "could not find function", run
# devtools::install() first to sync the installed version with development code.
prediction_matrices <- function(noFolds, ordered_data, model, technique, cores) {
  out <- tryCatch(
    {
      # LOOCV: set noFolds to number of observations
      is_loocv <- is.null(noFolds)
      if (is_loocv) {
        noFolds <- nrow(ordered_data)
      }
      folds <- cut(seq(1, nrow(ordered_data)), breaks = noFolds, labels = FALSE)

      # Use parallel execution only when explicitly requested via cores parameter
      use_parallel <- !is.null(cores)

      if (use_parallel) {
        cl <- setup_parallel_cluster(cores)

        # Export helper functions defined in this file to the worker environments
        parallel::clusterExport(cl = cl, varlist = c("generate_lm_predictions",
                                                     "predict_lm_matrices",
                                                     "standardize_data",
                                                     "unstandardize_data"), envir = environment())

        utils::capture.output(
          matrices <- parallel::parSapply(
            cl, 1:noFolds, in_and_out_sample_predictions, folds = folds,
            ordered_data = ordered_data,
            model = model,
            technique = technique
          )
        )
        parallel::stopCluster(cl)
      } else {
        matrices <- sapply(1:noFolds, in_and_out_sample_predictions,
                           folds = folds, ordered_data = ordered_data,
                           model = model, technique = technique)
      }

      # collect the odd and even numbered matrices from the matrices return object
      no_int_mmvars <- model$mmVariables[!is_interaction(model$mmVariables)]
      in_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==1]])
      out_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==2]])
      in_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==3]])
      out_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==4]])
      in_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==5]])
      out_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==6]])
      PLS_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==7]])
      LM_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==0]])

      # mean the in-sample construct predictions by row
      average_insample_construct <- sapply(1:length(model$constructs), mean_rows, matrix = in_sample_construct_matrix,
                                           noFolds = noFolds,
                                           constructs = model$constructs)

      # mean the in-sample item predictions by row
      average_insample_item <- sapply(1:length(no_int_mmvars), mean_rows, matrix = in_sample_item_matrix,
                                      noFolds = noFolds,
                                      constructs = no_int_mmvars)

      # sum the out-sample construct predictions by row
      average_outsample_construct <- sapply(1:length(model$constructs), sum_rows, matrix = out_sample_construct_matrix,
                                            noFolds = noFolds,
                                            constructs = model$constructs)

      # sum the out-sample item predictions by row
      average_outsample_item <- sapply(1:length(no_int_mmvars), sum_rows, matrix = out_sample_item_matrix,
                                       noFolds = noFolds,
                                       constructs = no_int_mmvars)

      # square the out-sample pls residuals, mean them and take the square root
      average_insample_pls_item_residuals <- sqrt(sapply(1:length(no_int_mmvars), mean_rows, matrix = PLS_in_sample_item_residuals^2,
                                                         noFolds = noFolds,
                                                         constructs = no_int_mmvars))
      # Collect endogenous items
      endogenous_items <- unlist(sapply(all_endogenous(model$smMatrix), function(x) construct_items(model$mmMatrix, x)), use.names = FALSE)

      # mean the in-sample lm predictions by row
      average_insample_lm <- sapply(1:length(endogenous_items), mean_rows, matrix = in_sample_lm_matrix,
                                    noFolds = noFolds,
                                    constructs = endogenous_items)

      # sum the out-sample item predictions by row
      average_outsample_lm <- sapply(1:length(endogenous_items), sum_rows, matrix = out_sample_lm_matrix,
                                     noFolds = noFolds,
                                     constructs = endogenous_items)

      # square the out-sample lm residuals, mean them, and take square root
      average_insample_lm_item_residuals <- sqrt(sapply(1:length(endogenous_items), mean_rows, matrix = LM_in_sample_item_residuals^2,
                                                        noFolds = noFolds,
                                                        constructs = endogenous_items))

      colnames(average_insample_construct) <- colnames(average_outsample_construct) <- model$constructs
      colnames(average_insample_item) <- colnames(average_insample_pls_item_residuals) <- colnames(average_outsample_item) <- no_int_mmvars
      colnames(average_insample_lm) <- colnames(average_outsample_lm) <- colnames(average_insample_lm_item_residuals) <- endogenous_items

      return(list(out_of_sample_construct = average_outsample_construct,
                  in_sample_construct = average_insample_construct,
                  out_of_sample_item = average_outsample_item,
                  in_sample_item = average_insample_item,
                  out_of_sample_lm_item = average_outsample_lm,
                  in_sample_lm_item = average_insample_lm,
                  pls_in_sample_item_residuals = average_insample_pls_item_residuals,
                  lm_in_sample_item_residuals = average_insample_lm_item_residuals))
    },
    error=function(cond) {
      message("Cross-validation encountered this ERROR: ")
      message(cond)
      if (exists("cl")) parallel::stopCluster(cl)
      return(NULL)
    },
    warning=function(cond) {
      message("Cross-validation encountered this WARNING:")
      message(cond)
      if (exists("cl")) parallel::stopCluster(cl)
      return(NULL)
    },
    finally={
      #
    }
  )
}

# Function to return the RMSE and MAE of a score
prediction_metrics <- function(residuals) {
  RMSE <- sqrt(mean(residuals^2))
  MAE <- mean(abs(residuals))
  return(matrix(c(RMSE,MAE), nrow = 2, ncol = 1, byrow = TRUE))
}

predict_lm_matrices <- function(x, depTrainData, indepTrainData,indepTestData, endogenous_items) {
  # Train LM
  trainLM <- stats::lm(depTrainData[,x] ~ ., indepTrainData)
  # Predict out of sample
  lmprediction_out_sample <- stats::predict(trainLM, newdata = indepTestData)
  # Predict in sample
  lmprediction_in_sample <- stats::predict(trainLM, newdata = indepTrainData)
  return(list(lm_prediction_in_sample = lmprediction_in_sample,
              lm_prediction_out_sample = lmprediction_out_sample))
}

generate_lm_predictions <- function(x, model, ordered_data, testIndexes, endogenous_items, trainIndexes, technique) {
  # Extract the target and non-target variables for Linear Model
  dependant_items <- construct_items(model$mmMatrix, x)

  # Create matrix return object holders
  in_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))
  out_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))

  # Select the correct independent variables to be included in independent matrix
  # for predict_DA this would be the indicators of the direct antecedents only
  # for predict_EA this would be the indicators of the earliest antecedents only
  if (identical(technique, predict_DA)) {
    focal_construct_antecedents <- construct_antecedents(model$smMatrix, x)
    focal_construct_antecedent_items <- unlist(sapply(focal_construct_antecedents, function (focal) construct_items(model$mmMatrix, focal)))
  }
  else {
    focal_construct_antecedents <- only_exogenous(model$smMatrix)
    focal_construct_antecedent_items <- unlist(sapply(focal_construct_antecedents, function (focal) construct_items(model$mmMatrix, focal)))
  }
  independant_matrix <- ordered_data[ , focal_construct_antecedent_items,drop = F]
  dependant_matrix <- as.matrix(ordered_data[,dependant_items, drop = F])

  # Create independant items matrices - training and testing
  indepTestData <- independant_matrix[testIndexes, ,drop = F]
  indepTrainData <- independant_matrix[-testIndexes, ,drop = F]

  # Create dependant matrices - training and testing
  # if (length(testIndexes) == 1) {
    # depTestData <- t(as.matrix(dependant_matrix[testIndexes, ,drop = F]))
  # } else {
    depTestData <- as.matrix(dependant_matrix[testIndexes, ,drop = F])
  # }
  depTrainData <- as.matrix(dependant_matrix[-testIndexes, ])
  colnames(depTrainData) <- colnames(depTestData) <- dependant_items

  lm_prediction_list <- sapply(dependant_items, predict_lm_matrices, depTrainData = depTrainData,
                               indepTrainData = indepTrainData,
                               indepTestData = indepTestData,
                               endogenous_items = endogenous_items)
  in_sample_matrix[trainIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==1]]), ncol = length(dependant_items), nrow = nrow(depTrainData), dimnames = list(rownames(depTrainData),dependant_items))
  out_sample_matrix[testIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==0]]), ncol = length(dependant_items), nrow = nrow(depTestData), dimnames = list(rownames(depTestData),dependant_items))

  return(list(in_sample_matrix, out_sample_matrix))
}

#' Predictive Scheme
#'
#' \code{predict_EA} and \code{predict_DA} specify the predictive scheme to be used in the generation of the
#' predictions. EA refers to Earliest Antecedents nad DA to Direct Antecedents.
#'
#' @param smMatrix is the \code{structural_model} - a source-to-target matrix representing the inner/structural model,
#'  generated by \code{relationships} generated by SEMinR.
#'
#' @param path_coef is the Path Coefficients matrix from a SEMinR model.
#'
#' @param construct_scores is the matrix of construct scores generated by SEMinR.
#'
#' @usage
#'  predict_EA(smMatrix, path_coef, construct_scores)
#'
#' @export
predict_EA <- function(smMatrix, path_coef, construct_scores) {
  order <- construct_order(smMatrix)
  only_exo <- only_exogenous(smMatrix)
  return_matrix <- construct_scores
  return_matrix[,order] <- 0
  for (construct in order) {
    return_matrix[,construct] <- return_matrix %*% path_coef[,construct]

  }
  return(return_matrix)
}

#' Predictive Scheme
#'
#' \code{predict_EA} and \code{predict_DA} specify the predictive scheme to be used in the generation of the
#' predictions. EA refers to Earliest Antecedents nad DA to Direct Antecedents.
#'
#' @param smMatrix is the \code{structural_model} - a source-to-target matrix representing the inner/structural model,
#'  generated by \code{relationships} generated by SEMinR.
#'
#' @param path_coef is the Path Coefficients matrix from a SEMinR model.
#'
#' @param construct_scores is the matrix of construct scores generated by SEMinR.
#'
#' @usage
#'  predict_DA(smMatrix, path_coef, construct_scores)
#'
#' @export
predict_DA <- function(smMatrix, path_coef, construct_scores) {
  only_exo <- only_exogenous(smMatrix)
  return_matrix <- construct_scores%*%path_coef
  return_matrix[,only_exo] <- construct_scores[,only_exo]
  return(return_matrix)
}

return_predict_error <- function(object, indicator) {
  object$prediction_error[,indicator]
}
