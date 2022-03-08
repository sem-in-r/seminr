#' @export
summary.cfa_model <- function(object, na.print=".", digits=3, alpha=0.05,...) {
  stopifnot(inherits(object, "cfa_model"))
  #TODO: we should set the package attribute to seminr rather than as class attribute
  stopifnot(inherits(object, "seminr_model"))

  model_summary     <- summarize_cb_measurement(object, alpha=alpha)
  model_summary$quality <- list(
    fit = summarize_fit(object$lavaan_output),
    reliability = rhoC_AVE(object)
  )

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
  curated_fit <- x$quality$fit$curated
  print(curated_fit$ordinary, digits=digits+1)
  if (!is.null(curated_fit$robust)) print(curated_fit$robust, digits=digits+1)

  cat("\n")
  cat(" Loadings:\n")
  print(x$loadings, na.print=na.print, digits=digits)
  invisible(x)
}
