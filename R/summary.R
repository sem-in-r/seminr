# summary function for seminr
#' @export
summary.seminr_model <- function(model, na.print=".", digits=3, ...) {
  stopifnot(inherits(model, "seminr_model"))
  path_reports <- report_paths(model, digits)
  metrics <- evaluate_model(model)
  iterations <- model$iterations

  model_summary <- list(iterations=iterations,
                        paths=path_reports,
                        metrics=metrics,
                        loadings=model$outer_loadings,
                        cross_loadings=metrics$Validity$`Cross-Loadings`,
                        weights=model$outer_weights,
                        reliability=metrics$Reliability)
  class(model_summary) <- "summary.seminr_model"
  model_summary
}

# print summary function for seminr
#' @export
print.summary.seminr_model <- function(summarized, na.print=".", digits=3, ...) {
  cat("\n", sprintf("Total Iterations: %s", summarized$iterations))

  cat("\nPath Coefficients:\n")
  print(summarized$paths, na.print = na.print, digits=digits)

  cat("\nReliability:\n")
  print(summarized$reliability, na.print = na.print, digits=digits)

  cat("\n")
  invisible(summarized)
}

# Summary for bootstrapped seminr model
#' @export
summary.boot_seminr_model <- function(boot_model, ...) {
  stopifnot(inherits(boot_model, "boot_seminr_model"))
  boot_matrix <- boot_model$bootstrapMatrix
  n <- nrow(boot_mobi_pls$data)

  # REFACTOR: Extract mean and SE columns from boot_matrix
  num_endogenous <- ncol(boot_matrix) / 3
  boot_mean <- boot_matrix[, c((num_endogenous+1):(2*num_endogenous))]
  boot_SE <- boot_matrix[, c((2*num_endogenous+1):(3*num_endogenous))]

  # calculate t-values and two-tailed p-values; 0 paths become NaN
  boot_t <- boot_mean / boot_SE
  boot_p <- 2*pt(boot_t, df = n-1, lower.tail = FALSE)

  # REFACTOR: removing " Boot Mean" suffix from column names
  colnames(boot_t) <- substr(colnames(boot_t), 1, nchar(colnames(boot_t))-10)
  colnames(boot_p) <- substr(colnames(boot_p), 1, nchar(colnames(boot_p))-10)
  boot_t[is.nan(boot_t)] <- NA
  boot_p[is.nan(boot_p)] <- NA

  boot_summary <- list(nboot = boot_model$boots, t_values = boot_t, p_values = boot_p)
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
print.summary.boot_seminr_model <- function(summarized, na.print=".", digits=3, ...) {
  cat("\n", sprintf("Bootstrapped resamples: %s", summarized$nboot))

  cat("\n\nStructural Path t-values:\n")
  print_matrix(summarized$t_values, na.print, digits)

  cat("\n\nStructural Path p-values:\n")
  print_matrix(summarized$p_values, na.print, digits)

  cat("\n")
  invisible(summarized)
}
