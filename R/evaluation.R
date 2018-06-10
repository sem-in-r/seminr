evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  out <- list(rel,val)
  names(out) <- c("reliability","validity")
  return(out)
}

# Reliability ----
reliability <- function(seminr_model) {
  mat1 <- rhoC_AVE(seminr_model)
  mat2 <- rho_A(seminr_model)
  return(cbind(mat1,mat2))
}

# Validity ----
validity <- function(seminr_model) {
  list(
    # htmt            = HTMT(seminr_model),
    cross_loadings  = cross_loadings(seminr_model),
    item_vifs       = item_vifs(seminr_model),
    antecedent_vifs = antecedent_vifs(seminr_model)
  )
}

cross_loadings <- function(seminr_model) {
  return(stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$construct_scores))
}

# Measurement Model Evaluation ----
#' @export
evaluate_measurement_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "seminr_model"))

  # Collect construct types
  factors <- get_factors(object)
  composites <- get_composites(object)
  factor_items <- sapply(factors,items_of_construct,object)
  composite_items <- unlist(sapply(composites,items_of_construct,object))

  # get metrics object
  metrics <- evaluate_model(object)
  # Get factor metrics ----

  # If only one factor
  if (length(factors) == 1) {
    factor_reliability <- as.matrix(t(metrics$reliability[factors,c("AVE","rhoA")]))
    rownames(factor_reliability) <- factors

    factor_indicator_reliability <- as.matrix(object$outer_loadings[factor_items,factors])
    colnames(factor_indicator_reliability) <- factors

    discriminant_validity <- HTMT(object)
  # If many factors
  } else if (length(factors) > 1) {
    factor_reliability <- metrics$reliability[factors,c("AVE","rhoA")]
    factor_indicator_reliability <- object$outer_loadings[factor_items,factors]
    discriminant_validity <- HTMT(object)
  # If no factors
  } else {
    factor_reliability <- NA
    factor_indicator_reliability <- NA
    discriminant_validity <- NA
  }

  # Get composite metrics ----

  # If only one composite
  if (length(composites) == 1) {
    composite_indicator_reliability <- as.matrix(object$outer_weights[composite_items,composites])
    colnames(composite_indicator_reliability) <- composites

    # If many composites
  } else if (length(composites) > 1) {
    composite_indicator_reliability <- object$outer_weights[composite_items,composites]

    # If no composites
  } else {
    composite_indicator_reliability <- NA

  }

  # Measurement model
  cat("\nMeasurement Model Evaluation:\n")

  # First report Factor metrics:
  cat("\n------------------Factors:------------------\n")

  cat("1. Indicator Reliability:\nLoadings:\n")
  print(factor_indicator_reliability, na.print = na.print, digits=digits)

  cat("\n2. Factor Reliability and Convergent Validity:\n")
  print(factor_reliability, na.print = na.print, digits=digits)
  #cat("\n")

  cat("\n3. Discriminant Validity\n")
  cat("HTMT\n")
  print(discriminant_validity, na.print = na.print, digits=digits)

  # First report Factor metrics:
  cat("\n------------------Composites:---------------\n")

  cat("1. Indicator Reliability:\nWeights:\n")
  print(composite_indicator_reliability, na.print = na.print, digits=digits)

  cat("\n2. Collinearity:\nItem VIFs per Construct:\n")
  print(metrics$validity$item_vifs, na.print = na.print, digits=digits)
  #cat("\n")

  #measurement_model_summary <- list(factor_reliability = factor_reliability,
  #                                  factor_indicator_reliability = factor_indicator_reliability,
  #                                  discriminant_validity = discriminant_validity)
  #class(measurement_model_summary) <- "measurement_model_evaluation.seminr_model"
  #measurement_model_summary
}

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


#### Stolen from testthat ----
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }

  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) {
    col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  }

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours <- c(
  "black" = "0;30",
  "blue" = "0;34",
  "green" = "0;32",
  "cyan" = "0;36",
  "red" = "0;31",
  "purple" = "0;35",
  "brown" = "0;33",
  "light gray" = "0;37",
  "dark gray" = "1;30",
  "light blue" = "1;34",
  "light green" = "1;32",
  "light cyan" = "1;36",
  "light red" = "1;31",
  "light purple" = "1;35",
  "yellow" = "1;33",
  "white" = "1;37"
)

.bg_colours <- c(
  "black" = "40",
  "red" = "41",
  "green" = "42",
  "brown" = "43",
  "blue" = "44",
  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

rcmd_running <- function() {
  nchar(Sys.getenv('R_TESTS')) != 0
}
