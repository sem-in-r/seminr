evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  # Coerce reliability of interaction to 1
  #rel[grepl("\\*", rownames(rel)),] <- 1
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
evaluate_measurement_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "seminr_model"))

  # Collect construct types
  factors <- get_factors(object)
  composites <- get_composites(object)
  factor_items <- unlist(sapply(factors,items_of_construct,object))
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
  print(metrics$validity$item_vifs[composites], na.print = na.print, digits=digits)
  #cat("\n")

  measurement_model_evaluation <- list(factor_reliability = factor_reliability,
                                       factor_indicator_reliability = factor_indicator_reliability,
                                       factor_discriminant_validity = discriminant_validity,
                                       composite_indicator_reliability = composite_indicator_reliability,
                                       composite_collinearity = metrics$validity$item_vifs[composites])
  class(measurement_model_evaluation) <- "measurement_model_evaluation.seminr_model"
  return(measurement_model_evaluation)
}

boot_evaluate_measurement_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "boot_seminr_model"))

  # Collect construct types
  factors <- get_factors(object)
  composites <- get_composites(object)
  factor_items <- unlist(sapply(factors,items_of_construct,object))
  composite_items <- unlist(sapply(composites,items_of_construct,object))

  # get metrics object
  metrics <- evaluate_model(object)

  # Evaluate weights boot matrix
  # REFACTOR: Extract endogenous column names, means, and SEs from boot_matrix
  weights_boot_matrix <- object$boot_weights
  num_composites <- ncol(weights_boot_matrix) / 3
  column_names <- colnames(weights_boot_matrix)[1:num_composites]
  weights_boot_mean <- as.matrix(weights_boot_matrix[, c((1*num_composites+1):(2*num_composites))])
  weights_boot_SE   <- as.matrix(weights_boot_matrix[, c((2*num_composites+1):(3*num_composites))])

  # calculate t-values and two-tailed p-values; 0 paths become NaN
  weights_boot_t <- abs(weights_boot_mean / weights_boot_SE)
  weights_boot_p <- 2*stats::pt(weights_boot_t, df = object$boots-1, lower.tail = FALSE)
  colnames(weights_boot_t) <- object$constructs
  colnames(weights_boot_p) <- object$constructs

  # Evaluate HTMT boot matrix
  # REFACTOR: Extract endogenous column names, means, and SEs from boot_matrix
  HTMT_boot_matrix <- object$boot_HTMT
  num_factors <- ncol(HTMT_boot_matrix) / 3
  factor_names <- colnames(HTMT_boot_matrix)[1:num_factors]
  HTMT_boot_mean <- as.matrix(HTMT_boot_matrix[, c((1*num_factors+1):(2*num_factors))])
  HTMT_boot_SE   <- as.matrix(HTMT_boot_matrix[, c((2*num_factors+1):(3*num_factors))])

  # calculate t-values and two-tailed p-values; 0 paths become NaN
  HTMT_boot_t <- abs((HTMT_boot_mean-1) / HTMT_boot_SE)
  HTMT_boot_p <- 2*stats::pt(HTMT_boot_t, df = object$boots-1, lower.tail = FALSE)

  colnames(HTMT_boot_t) <- colnames(HTMT(object))
  colnames(HTMT_boot_p) <- colnames(HTMT(object))

  discriminant_validity <- HTMT(object)
  discriminant_validity_t <- HTMT_boot_t
  discriminant_validity_p <- HTMT_boot_p
  # Get factor metrics ----

  # If only one factor
  if (length(factors) == 1) {
    factor_reliability <- as.matrix(t(metrics$reliability[factors,c("AVE","rhoA")]))
    rownames(factor_reliability) <- factors

    factor_indicator_reliability <- as.matrix(object$outer_loadings[factor_items,factors])
    colnames(factor_indicator_reliability) <- factors

    # If many factors
  } else if (length(factors) > 1) {
    factor_reliability <- metrics$reliability[factors,c("AVE","rhoA")]
    factor_indicator_reliability <- object$outer_loadings[factor_items,factors]
    # If no factors
  } else {
    factor_reliability <- NA
    factor_indicator_reliability <- NA
    discriminant_validity <- NA
    discriminant_validity_t <- NA
    discriminant_validity_p <- NA
  }

  # Get composite metrics ----

  # If only one composite
  if (length(composites) == 1) {
    composite_indicator_reliability <- as.matrix(object$outer_weights[composite_items,composites])
    composite_indicator_weights_t <- as.matrix(weights_boot_t[composite_items,composites])
    composite_indicator_weights_p <- as.matrix(weights_boot_p[composite_items,composites])

    colnames(composite_indicator_reliability) <- composites

    # If many composites
  } else if (length(composites) > 1) {
    composite_indicator_reliability <- object$outer_weights[composite_items,composites]
    composite_indicator_weights_t <- weights_boot_t[composite_items,composites]
    composite_indicator_weights_p <- weights_boot_p[composite_items,composites]

    # If no composites
  } else {
    composite_indicator_reliability <- NA
    composite_indicator_weights_t <- NA
    composite_indicator_weights_p <- NA
  }

  # Clean boot matrices
  composite_indicator_weights_t[is.nan(composite_indicator_weights_t)] <- NA
  composite_indicator_weights_p[is.nan(composite_indicator_weights_p)] <- NA

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
  cat("HTMT t-values:\n")
  print(discriminant_validity_t, na.print = na.print, digits=digits)
  cat("HTMT p-values:\n")
  print(discriminant_validity_p, na.print = na.print, digits=digits)

  # Second report Composite metrics:
  cat("\n------------------Composites:---------------\n")

  cat("1. Indicator Reliability:\nWeights:\n")
  print(composite_indicator_reliability, na.print = na.print, digits=digits)
  cat("\n Weights t-values:\n")
  print(composite_indicator_weights_t, na.print = na.print, digits=digits)
  cat("\n Weights p-values:\n")
  print(composite_indicator_weights_p, na.print = na.print, digits=digits)
  cat("\n2. Collinearity:\nItem VIFs per Construct:\n")
  print(metrics$validity$item_vifs[composites], na.print = na.print, digits=digits)
  #cat("\n")

  boot_measurement_model_evaluation <- list(factor_reliability = factor_reliability,
                                            factor_indicator_reliability = factor_indicator_reliability,
                                            factor_discriminant_validity = discriminant_validity,
                                            factor_discriminant_validity_t_values =  discriminant_validity_t,
                                            factor_discriminant_validity_p_values =  discriminant_validity_p,
                                            composite_indicator_reliability = composite_indicator_reliability,
                                            composite_indicator_weights_t_values = composite_indicator_weights_t,
                                            composite_indicator_weights_p_values = composite_indicator_weights_p,
                                            composite_collinearity = metrics$validity$item_vifs[composites])
  class(boot_measurement_model_evaluation) <- "measurement_model_evaluation.boot_seminr_model"
  return(boot_measurement_model_evaluation)
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
