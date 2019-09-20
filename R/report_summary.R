# summary function for seminr
#' @export
summary.seminr_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "seminr_model"))
  path_reports <- report_paths(object, digits)
  metrics <- evaluate_model(object)
  iterations <- object$iterations
  composite_scores <- return_only_composite_scores(object)
  model_summary <- list(iterations = iterations,
                        paths = path_reports,
                        loadings = object$outer_loadings,
                        weights = object$outer_weights,
                        cross_loadings = metrics$validity$cross_loadings,
                        vif_items = metrics$validity$item_vifs,
                        reliability = metrics$reliability,
                        composite_scores = composite_scores,
                        vif_antecedents = metrics$validity$antecedent_vifs)
  class(model_summary) <- "summary.seminr_model"
  model_summary
}

# print summary function for seminr
#' @export
print.summary.seminr_model <- function(x, na.print=".", digits=3, ...) {
  cat("\n", sprintf("Total Iterations: %s", x$iterations))

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

  boot_summary <- list(nboot = object$boots,
                       bootstrapped_paths = paths_summary,
                       bootstrapped_weights = weights_summary,
                       bootstrapped_loadings = loadings_summary,
                       bootstrapped_HTMT = htmt_summary)
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
  cat("\n", sprintf("Bootstrap resamples: %s", x$nboot))

  cat("\n\nBootstrapped Structural Paths:\n")
  print_matrix(x$bootstrapped_paths[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped Weights:\n")
  print_matrix(x$bootstrapped_weights[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped Loadings:\n")
  print_matrix(x$bootstrapped_loadings[,c(1,2,3,4,5,6)], na.print, digits)

  cat("\nBootstrapped HTMT:\n")
  print_matrix(x$bootstrapped_HTMT[,c(1,2,3,5,6)], na.print, digits)

  cat("\n")
  invisible(x)
}
