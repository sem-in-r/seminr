#' @export
summary.cfa_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "cfa_model"))
  #TODO: we should set the package attribute to seminr rather than as class attribute
  stopifnot(inherits(object, "seminr_model"))

  model_summary     <- summarize_cb_measurement(object)
  model_summary$fit <- summarize_fit(object$lavaan_model)

  class(model_summary) <- c("summary.cfa_model", class(model_summary))
  model_summary
}

#' @export
print.summary.cfa_model <- function(x, na.print=".", digits=2, ...) {
  cat("\n")
  print_pkginfo("Results from", x$meta$seminr)
  print_pkginfo("Estimation used", x$meta$seminr)

  cat("\n")
  cat(" Fit metrics:\n")
  print(x$fit$curated$ordinary, digits=digits)
  if (!is.null(x$fit$curated$robust)) print(x$fit$curated$robust)

  cat("\n")
  cat(" Loadings:\n")
  print(x$loadings, na.print=na.print, digits=digits)
  invisible(x)
}
