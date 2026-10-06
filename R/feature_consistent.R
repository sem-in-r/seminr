#' seminr PLSc Function
#'
#' The \code{PLSc} function calculates the consistent PLS path coefficients and loadings for
#' a common-factor model. It returns a \code{seminr_model} containing the adjusted and consistent
#' path coefficients and loadings for common-factor models and composite models.
#' Only common-factor (\code{reflective()}) constructs are corrected for measurement error;
#' composites of any mode are treated as fully reliable (rho_A = 1), as in Dijkstra and
#' Henseler (2015). A higher-order composite of common factors is corrected with the
#' reliability of its stage-2 proxy, a weighted sum of error-laden lower-order construct
#' scores (van Riel et al., 2017).
#'
#' @param seminr_model A \code{seminr_model} containing the estimated seminr model.
#'
#' @return A SEMinR model object which has been adjusted according to PLSc.
#'
#' @usage
#' PLSc(seminr_model)
#'
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interaction_term}}
#'          \code{\link{bootstrap_model}}
#'
#' @references Dijkstra, T. K., & Henseler, J. (2015). Consistent partial least squares path modeling. \emph{MIS Quarterly}, 39(2), 297--316.
#'
#' van Riel, A. C. R., Henseler, J., Kemény, I., & Sasovova, Z. (2017). Estimating hierarchical constructs
#' using consistent partial least squares: The case of second-order composites of common factors.
#' \emph{Industrial Management & Data Systems}, 117(3), 459--477.
#'
#' @examples
#' mobi <- mobi
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' seminr_model <- estimate_pls(data = mobi,
#'                              measurement_model = mobi_mm,
#'                              structural_model = mobi_sm)
#'
#' PLSc(seminr_model)
#' @export
PLSc <- function(seminr_model) {
  # Function to implement PLSc as per Dijkstra, T. K., & Henseler, J. (2015). Consistent partial least squares path modeling. MIS Quarterly, 39(2), 297-316.
  # get relevant parts of the estimated model
  smMatrix <- seminr_model$smMatrix
  mmMatrix <- seminr_model$mmMatrix
  path_coef <- seminr_model$path_coef
  loadings <- seminr_model$outer_loadings
  rSquared <- seminr_model$rSquared
  construct_scores <- seminr_model$construct_scores
  # Calculate rho_A for adjustments and adjust the correlation matrix
  disattenuated <- plsc_disattenuation(seminr_model)
  rho <- disattenuated$rho
  adj_construct_score_cors <- disattenuated$construct_cors

  # iterate over endogenous constructs and adjust path coefficients and R-squared
  for (i in all_endogenous(smMatrix)) {

    #Indentify the exogenous variables
    exogenous <- construct_antecedents(smMatrix, i)

    #Solve the system of equations
    results <- solve(adj_construct_score_cors[exogenous, exogenous],
                    adj_construct_score_cors[exogenous, i])
    # Assign the path names
    names(results) <- exogenous

    #Assign the Beta Values to the Path Coefficient Matrix
    path_coef[exogenous, i] <- results
  }

  #calculate insample metrics
  rSquared <- metrics_insample(seminr_model$data, construct_scores, smMatrix, all_endogenous(smMatrix), adj_construct_score_cors)

  # get all common-factor constructs (Mode A Consistent) in a vector
  reflectives <- intersect(all_reflective(mmMatrix), construct_names(seminr_model$smMatrix))

  # function to adjust the loadings of a common-factor
  adjust_loadings <- function(i) {
    items <- construct_items(mmMatrix, i)
    w <- as.matrix(seminr_model$outer_weights[items, i])
    loadings[items, i] <- w %*% (sqrt(rho[i]) / t(w) %*% w )
    loadings[, i]
  }

  # apply the function over common-factors and assign to loadings matrix
  if(length(reflectives) > 0) {
    loadings[, reflectives] <- sapply(reflectives, adjust_loadings)
  }

  # Assign the adjusted values for return
  seminr_model$path_coef <- path_coef
  seminr_model$outer_loadings <- loadings
  seminr_model$rSquared <- rSquared
  return(seminr_model)
}

# Classed PLSc condition ----
# Callers can handle these by class: cross-validation skips a training fold
# whose PLSc solution is inadmissible (see in_and_out_sample_predictions())
plsc_condition <- function(message, class) {
  structure(class = c(class, "condition"), list(message = message, call = NULL))
}

# Construct reliabilities and disattenuated construct correlations for PLSc ----
#
# Shared by PLSc() and PLSc prediction (plsc_implied_correlations()), so that
# estimation and prediction always correct with the same rho. Only common
# factors are corrected: composites (any mode) and interaction terms are taken
# as fully reliable (rho = 1), as in Dijkstra & Henseler (2015), except
# higher-order composites of common factors (see hoc_composite_reliability()).
#
# @param seminr_model  An estimated seminr_model
# @return list(rho = named rho vector, construct_cors = disattenuated construct
#   correlation matrix with unit diagonal)
plsc_disattenuation <- function(seminr_model) {
  constructs <- constructs_in_model(seminr_model)$construct_names
  rho <- rho_A(seminr_model, constructs)[, 1]
  rho[is_interaction(constructs) | !(constructs %in% all_factors(seminr_model))] <- 1
  for (hoc in higher_order_composites(seminr_model, constructs)) {
    rho[hoc] <- hoc_composite_reliability(seminr_model, hoc)
  }
  not_positive <- !is.finite(rho) | rho <= 0
  if (any(not_positive)) {
    stop(plsc_condition(
      paste0("PLSc cannot correct this model: rho_A is not positive for ",
             paste(sprintf("%s (%.3f)", constructs[not_positive], rho[not_positive]), collapse = ", "),
             ". The correction divides by sqrt(rho_A), so the paths, R-squared and loadings ",
             "would be undefined. Check the signs and correlations of these constructs' items, ",
             "or estimate them as composites."),
      c("seminr_inadmissible_plsc", "error")))
  }
  construct_cors <- stats::cor(seminr_model$construct_scores[, constructs]) / sqrt(outer(rho, rho))
  diag(construct_cors) <- 1
  list(rho = rho, construct_cors = construct_cors)
}

# Reliability of a higher-order composite (two-stage) ----
#
# At stage 2 a higher-order composite is a weighted sum of its LOC scores. When
# LOCs are common factors their scores carry measurement error, so the HOC proxy
# is not error-free: its reliability is w' S* w / w' S w, where S is the
# correlation matrix of the LOC scores and S* replaces its diagonal with the
# LOCs' stage-1 reliabilities (van Riel et al., 2017; as in cSEM's two-stage
# approach). LOCs that are composites have reliability 1, so a HOC of
# composites is not corrected.
#
# @param seminr_model  The stage-2 seminr_model, with first_stage_model attached
# @param constructs    Constructs to search
# @return Names of the constructs whose indicators are LOC scores
higher_order_composites <- function(seminr_model, constructs) {
  first_stage <- seminr_model$first_stage_model
  if (is.null(first_stage)) return(character(0))
  is_hoc <- vapply(constructs, function(construct) {
    !(construct %in% all_factors(seminr_model)) && !is_interaction(construct) &&
      all(construct_items(seminr_model$mmMatrix, construct) %in% first_stage$constructs)
  }, logical(1))
  constructs[is_hoc]
}

hoc_composite_reliability <- function(seminr_model, hoc) {
  first_stage <- seminr_model$first_stage_model
  locs <- construct_items(seminr_model$mmMatrix, hoc)
  w <- seminr_model$outer_weights[locs, hoc]
  S <- stats::cor(first_stage$construct_scores[, locs])
  S_star <- S
  diag(S_star) <- plsc_disattenuation(first_stage)$rho[locs]
  drop(t(w) %*% S_star %*% w / (t(w) %*% S %*% w))
}

# Function to implement PLSc as per Dijkstra, T. K., & Henseler, J. (2015). Consistent partial least squares path modeling. MIS Quarterly, 39(2), 297-316.
# Interactions are detected from the construct names: estimate_pls() sets
# $interaction only after this runs
model_consistent <- function(seminr_model) {
  if (!has_reflective(seminr_model)) {
    return(seminr_model)
  }
  if (any(is_interaction(seminr_model$constructs))) {
    message(
      "Models with interactions can be estimated as PLS consistent, but are subject to some bias as per Becker et al. (2018)\n",
      "'Estimating Moderating Effects in PLS-SEM and PLSc-SEM: Interaction Term Generation*Data Treatment'")
  }
  PLSc(seminr_model)
}
