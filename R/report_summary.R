# summary function for seminr
#' @export
summary.seminr_model <- function(object, ...) {
  stopifnot(inherits(object, "seminr_model"))

  path_reports <- report_paths(object)
  metrics <- evaluate_model(object)
  iterations <- object$iterations
  composite_scores <- return_only_composite_scores(object)
  descriptives <- descriptives(object)
  fSquare <- model_fsquares(object)
  model_summary <- list(
    meta = list(seminr = seminr_info()), # other estimation engines could go here in future
    iterations = iterations,
    paths = path_reports,
    loadings = convert_to_table_output(object$outer_loadings),
    weights = convert_to_table_output(object$outer_weights),
    validity = list(vif_items = metrics$validity$item_vifs,
                    htmt = t(metrics$validity$htmt),
                    fl_criteria = metrics$validity$fl_criteria,
                    cross_loadings = metrics$validity$cross_loadings),
    reliability = metrics$reliability,
    composite_scores = composite_scores,
    vif_antecedents = metrics$validity$antecedent_vifs,
    fSquare = fSquare,
    descriptives = descriptives,
    it_criteria = calculate_itcriteria(object)
  )
  class(model_summary) <- "summary.seminr_model"
  model_summary
}

# print summary function for seminr
#' @export
print.summary.seminr_model <- function(x, na.print=".", digits=3, ...) {
  cat("\n")
  print_pkginfo("Results from", x$meta$seminr)
  # cat("Total Iterations: ", x$iterations)

  cat("\nPath Coefficients:\n")
  print(x$paths, na.print = na.print, digits=digits)

  cat("\nReliability:\n")
  print(x$reliability, na.print = na.print, digits=digits)

  cat("\n")
  invisible(x)
}

# Summary for bootstrapped seminr model
#' @export
summary.boot_seminr_model <- function(object, alpha = 0.05, ...) {
  stopifnot(inherits(object, "boot_seminr_model"))
  boot_matrix <- object$paths_descriptives
  n <- nrow(object$data)

  # bootstrapped direct paths
  paths_summary <- parse_boot_array(object$path_coef, object$boot_paths, alpha = alpha)
  # bootstrapped weights
  weights_summary <- parse_boot_array(object$outer_weights, object$boot_weights, alpha = alpha)
  # bootstrapped loadings
  loadings_summary <- parse_boot_array(object$outer_loadings, object$boot_loadings, alpha = alpha)
  # bootstrapped HTMT
  htmt_summary <- parse_boot_array(HTMT(object), object$boot_HTMT, alpha = alpha)
  # bootstrapped total paths
  total_paths_summary <- parse_boot_array(total_effects(object$path_coef), object$boot_total_paths, alpha = alpha)

  boot_summary <- list(nboot = object$boots,
                       bootstrapped_paths = paths_summary,
                       bootstrapped_weights = weights_summary,
                       bootstrapped_loadings = loadings_summary,
                       bootstrapped_HTMT = htmt_summary,
                       bootstrapped_total_paths = total_paths_summary)
  class(boot_summary) <- "summary.boot_seminr_model"
  boot_summary
}

# formatting for print functions
print_matrix <- function(pmatrix, na.print=".", digits=3) {
  pmatrix[!is.na(pmatrix)] <- sprintf("%.*f", digits, pmatrix[!is.na(pmatrix)])
  print(pmatrix, na.print = na.print, digits=digits, quote = FALSE, right = TRUE)
}

# Print for summary of bootstrapped seminr model
#' @export
print.summary.boot_seminr_model <- function(x, na.print=".", digits=3, ...) {
  cat("\n")
  print_pkginfo("Results from", x$meta$seminr)
  cat("Bootstrap resamples: ", x$nboot)

  cat("\n\nBootstrapped Structural Paths:\n")
  print_matrix(x$bootstrapped_paths[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped Weights:\n")
  print_matrix(x$bootstrapped_weights[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped Loadings:\n")
  print_matrix(x$bootstrapped_loadings[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped HTMT:\n")
  print_matrix(x$bootstrapped_HTMT[,c(1,2,3,5,6)], na.print, digits)

  cat("\nBootstrapped Total Paths:\n")
  print_matrix(x$bootstrapped_total_paths[,c(1,2,3,5,6)], na.print, digits)

  cat("\n")
  invisible(x)
}

#' @export
print.table_output <- function(x, na.print=".", digits=3, ...) {
  class(x) <- "matrix"
  print_matrix(x, na.print = na.print, digits=digits)
  if(length(comment(x)) > 0) {
    cat("\n")
    cat(comment(x))
    cat("\n")
  }
  invisible(x)
}

#' @export
print.list_output <- function(x, na.print=".", digits=4, ...) {
  class(x) <- "list"
  print.listof(x, na.print = na.print, digits=digits)
  if(length(comment(x)) > 0) {
    cat("\n")
    cat(comment(x))
    cat("\n")
  }
  invisible(x)
}
# Summary for predicted seminr model
#' @export
summary.predict_pls_model <- function(object, alpha = 0.05, ...) {
  stopifnot(inherits(object, "predict_pls_model"))

  # Item evaluation
  item_evaluation <- item_metrics(object)

  model_summary <- list(PLS_in_sample = item_evaluation$PLS_item_prediction_metrics_IS,
                        PLS_out_of_sample = item_evaluation$PLS_item_prediction_metrics_OOS,
                        LM_in_sample = item_evaluation$LM_item_prediction_metrics_IS,
                        LM_out_of_sample = item_evaluation$LM_item_prediction_metrics_OOS,
                        prediction_error = object$PLS_out_of_sample_residuals)
  class(model_summary) <- "summary.predict_pls_model"
  return(model_summary)
}

# Print summary method for PLSpredict
#' @export
print.summary.predict_pls_model <- function(x, na.print=".", digits=3, ...) {
  stopifnot(inherits(x, "summary.predict_pls_model"))

  # Print the item metrics PLS & LM
  cat("\nPLS in-sample metrics:\n")
  print(x$PLS_in_sample, na.print = na.print, digits = digits)

  cat("\nPLS out-of-sample metrics:\n")
  print(x$PLS_out_of_sample, na.print = na.print, digits = digits)

  cat("\nLM in-sample metrics:\n")
  print(x$LM_in_sample, na.print = na.print, digits = digits)

  cat("\nLM out-of-sample metrics:\n")
  print(x$LM_out_of_sample, na.print = na.print, digits = digits)

  invisible(x)
}

# Plot summary method for PLSpredict
#' @export
plot.summary.predict_pls_model <- function(x, indicator) {
  stopifnot(inherits(x, "summary.predict_pls_model"))

  # Plot the indicator error
  plot(density(seminr:::return_predict_error(x,indicator)),
       main = paste("Distribution of predictive error of", indicator))
  # Grid
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
}
