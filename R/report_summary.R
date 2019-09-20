# summary function for seminr
#' @export
summary.seminr_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "seminr_model"))
  path_reports <- report_paths(object, digits)
  metrics <- evaluate_model(object)
  iterations <- object$iterations
  composite_scores <- return_only_composite_scores(object)
  descriptives <- descriptives(object)
  model_summary <- list(iterations = iterations,
                        paths = path_reports,
                        loadings = object$outer_loadings,
                        weights = object$outer_weights,
                        cross_loadings = metrics$validity$cross_loadings,
                        vif_items = metrics$validity$item_vifs,
                        reliability = metrics$reliability,
                        composite_scores = composite_scores,
                        vif_antecedents = metrics$validity$antecedent_vifs,
                        htmt = metrics$validity$htmt,
                        descriptives = descriptives)
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
summary.boot_seminr_model <- function(object, ...) {
  stopifnot(inherits(object, "boot_seminr_model"))
  boot_matrix <- object$paths_descriptives
  n <- nrow(object$data)

  # REFACTOR: Extract endogenous column names, means, and SEs from boot_matrix
  num_endogenous <- ncol(boot_matrix) / 3
  column_names <- colnames(boot_matrix)[1:num_endogenous]
  endogenous_names <- as.vector(substr(column_names, 1, nchar(column_names)-nchar(" PLS Est.")))
  boot_mean <- as.matrix(boot_matrix[, c((1*num_endogenous+1):(2*num_endogenous))])
  boot_SE   <- as.matrix(boot_matrix[, c((2*num_endogenous+1):(3*num_endogenous))])

  # calculate t-values and two-tailed p-values; 0 paths become NaN
  boot_t <- abs(boot_mean / boot_SE)
  boot_p <- 2*stats::pt(boot_t, df = object$boots-1, lower.tail = FALSE)

  colnames(boot_t) <- endogenous_names
  colnames(boot_p) <- endogenous_names
  boot_t[is.nan(boot_t)] <- NA
  boot_p[is.nan(boot_p)] <- NA

  boot_summary <- list(nboot = object$boots, t_values = boot_t, p_values = boot_p)
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
  cat("\n", sprintf("Bootstrapped resamples: %s", x$nboot))

  cat("\n\nStructural Path t-values:\n")
  print_matrix(x$t_values, na.print, digits)

  cat("\nStructural Path p-values:\n")
  print_matrix(x$p_values, na.print, digits)

  cat("\n")
  invisible(x)
}

# Print for generic tables of output
#' @export
print.table_output <- function(x, na.print=".", digits=3, ...) {
  class(x) <- "matrix"
  print(x, na.print = na.print, digits=digits)
  cat("\n")
  invisible(x)
}
