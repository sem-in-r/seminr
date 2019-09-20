#### fix here ----
## measurement_model_evaluation <- function(x) UseMethod("measurement_model_evaluation", x)
##print.measurement_model_evaluation <- function(x) c("measurement_model_evaluation", NextMethod())

# print measurement model evaluation function for seminr
#' @export
print.measurement_model_evaluation.seminr_model <- function(x, na.print=".", digits=3, ...) {

  # Structural Model
  #cat("\nStructural Model Evaluation:\n")
  #cat("\nPath Coefficients:\n")
  #print(x$paths, na.print = na.print, digits=digits)

  # Measurement model
  cat("\nMeasurement Model Evaluation:\n")

  # First report Factor metrics:
  cat("\nFactors:\n")

  cat("\nReliability:\n")
  cat("\n Indicator Reliability:\n")
  print(x$indicator_reliability, na.print = na.print, digits=digits)

  cat("\nFactor Reliability:\n")
  print(x$factor_reliability, na.print = na.print, digits=digits)
  #cat("\n")

  cat("\nDiscriminant Validity\n")
  print(x$discriminant_validity, na.print = na.print, digits=digits)

  # Then report composite metrics:
  #cat("\nComposites:\n")

  #cat("\nConfirmatory Composite Analysis:\n")

  #cat("\nCollinearity:\n")
  #print(x$vif_items, na.print = na.print, digits=digits)

  #cat("\nWeights:\n")
  #print(x$weights, na.print = na.print, digits=digits)

  invisible(x)
}
