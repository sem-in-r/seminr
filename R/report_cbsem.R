#' @export
summary.cbsem_model <- function(object, na.print=".", digits=3, alpha=0.05,...) {
  stopifnot(inherits(object, "cbsem_model"))
  #TODO: we should set the package attribute to seminr rather than as class attribute
  stopifnot(inherits(object, "seminr_model"))

  model_summary <- summarize_cb_measurement(object, alpha=alpha)
  regr_vifs = antecedent_vifs(
    object$smMatrix,
    model_summary$descriptives$correlations$constructs)

  model_summary$quality <- list(
    fit = summarize_fit(object$lavaan_output),
    reliability = rhoC_AVE(object),
    antecedent_vifs = regr_vifs
  )

  model_summary$paths <- summarize_cb_structure(object, alpha=alpha)

  class(model_summary) <- c("summary.cbsem_model", class(model_summary))
  model_summary
}

#' @export
print.summary.cbsem_model <- function(x, na.print=".", digits=2, ...) {
  cat("\n")
  print_pkginfo("Results from", x$meta$seminr)
  print_pkginfo("Estimation used", x$meta$seminr)

  cat("\nFit metrics:\n")
  print_matrix(x$quality$fit$curated$ordinary, na.print=na.print, digits=digits+1)
  if (!is.null(x$quality$fit$curated$robust)) {
    cat("\n")
    print_matrix(x$quality$fit$curated$robust, na.print=na.print, digits=digits+1)
  }

  cat("\nReliability:\n")
  print_matrix(x$quality$reliability, na.print=na.print, digits=digits)

  cat("\nPath Coefficients:\n")
  print_matrix(x$paths$coefficients, na.print=na.print, digits=digits)

  cat("\n")
  invisible(x)
}
