evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  # Coerce reliability of interaction to 1
  #rel[grepl("\\*", rownames(rel)), ] <- 1
  out <- list(rel,val)
  names(out) <- c("reliability", "validity")
  return(out)
}

# Reliability ----
reliability <- function(seminr_model) {
  #get HOC
  model_constructs <- constructs_in_model(seminr_model)
  alpha <- cronbachs_alpha(seminr_model, model_constructs$construct_names)
  mat1 <- rhoC_AVE(seminr_model, constructs = model_constructs$construct_names)
  mat2 <- rho_A(seminr_model, model_constructs$construct_names)
  table <- cbind(alpha, mat1, mat2)
  colnames(table) <- c("alpha", "rhoC",   "AVE",  "rhoA")
  comment(table) <- "Alpha, rhoC, and rhoA should exceed 0.7 while AVE should exceed 0.5"
  class(table) <- append(class(table), c("table_output","reliability_table"))
  return(table)
}

# Validity ----
validity <- function(seminr_model) {
  model_constructs <- constructs_in_model(seminr_model)
  list(
    htmt            = HTMT(seminr_model),
    cross_loadings  = cross_loadings(seminr_model, model_constructs),
    item_vifs       = item_vifs(seminr_model, model_constructs),
    # TODO: consider if antecedent vifs should be part of structural results
    antecedent_vifs = antecedent_vifs(seminr_model$smMatrix, stats::cor(seminr_model$construct_scores)),
    fl_criteria = fl_criteria_table(seminr_model, model_constructs)
  )
}

cross_loadings <- function(seminr_model, model_constructs) {
  if (is.null(seminr_model$hoc)) {
    all_mm_vars <- seminr_model$mmVariables
  } else {
    all_mm_vars <- all_items(seminr_model$measurement_model)
  }
  ret <- stats::cor(seminr_model$data[, all_mm_vars], model_constructs$construct_scores)
  convert_to_table_output(ret)
}

# Measurement Model Evaluation for tests----
evaluate_measurement_model <- function(object, na.print=".", digits=3, ...) {
  stopifnot(inherits(object, "seminr_model"))

  # Collect construct types
  factors <- get_factors(object)
  composites <- get_composites(object)
  factor_items <- unlist(sapply(factors, items_of_construct, object))
  composite_items <- unlist(sapply(composites, items_of_construct, object))

  # get metrics object
  metrics <- evaluate_model(object)
  # Get factor metrics ----

  # If only one factor
  if (length(factors) == 1) {
    factor_reliability <- as.matrix(t(metrics$reliability[factors, c("AVE", "rhoA")]))
    rownames(factor_reliability) <- factors

    factor_indicator_reliability <- as.matrix(object$outer_loadings[factor_items, factors])
    colnames(factor_indicator_reliability) <- factors

    discriminant_validity <- HTMT(object)
    # If many factors
  } else if (length(factors) > 1) {
    factor_reliability <- metrics$reliability[factors, c("AVE", "rhoA")]
    factor_indicator_reliability <- object$outer_loadings[factor_items, factors]
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
    composite_indicator_reliability <- as.matrix(object$outer_weights[composite_items, composites])
    colnames(composite_indicator_reliability) <- composites

    # If many composites
  } else if (length(composites) > 1) {
    composite_indicator_reliability <- object$outer_weights[composite_items, composites]

    # If no composites
  } else {
    composite_indicator_reliability <- NA

  }

  # Measurement model
  message("Measurement Model Evaluation:")

  # First report Factor metrics:
  message("------------------Factors:------------------")

  message("1. Indicator Reliability:\nLoadings:")
  message(factor_indicator_reliability)

  message("\n2. Factor Reliability and Convergent Validity:")
  message(factor_reliability)

  message("\n3. Discriminant Validity")
  message("HTMT")
  message(discriminant_validity)

  # First report Factor metrics:
  message("\n------------------Composites:---------------")

  message("1. Indicator Reliability:\nWeights:")
  message(composite_indicator_reliability)

  message("\n2. Collinearity:\nItem VIFs per Construct:")
  message(metrics$validity$item_vifs[composites])

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
  factor_items <- unlist(sapply(factors, items_of_construct, object))
  composite_items <- unlist(sapply(composites, items_of_construct, object))

  # get metrics object
  metrics <- evaluate_model(object)

  # Evaluate weights boot matrix
  # REFACTOR: Extract endogenous column names, means, and SEs from boot_matrix
  weights_boot_matrix <- object$weights_descriptives
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
  HTMT_boot_matrix <- object$HTMT_descriptives
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
    factor_reliability <- as.matrix(t(metrics$reliability[factors, c("AVE", "rhoA")]))
    rownames(factor_reliability) <- factors

    factor_indicator_reliability <- as.matrix(object$outer_loadings[factor_items, factors])
    colnames(factor_indicator_reliability) <- factors

    # If many factors
  } else if (length(factors) > 1) {
    factor_reliability <- metrics$reliability[factors, c("AVE", "rhoA")]
    factor_indicator_reliability <- object$outer_loadings[factor_items, factors]
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
    composite_indicator_reliability <- as.matrix(object$outer_weights[composite_items, composites])
    composite_indicator_weights_t <- as.matrix(weights_boot_t[composite_items, composites])
    composite_indicator_weights_p <- as.matrix(weights_boot_p[composite_items, composites])

    colnames(composite_indicator_reliability) <- composites

    # If many composites
  } else if (length(composites) > 1) {
    composite_indicator_reliability <- object$outer_weights[composite_items, composites]
    composite_indicator_weights_t <- weights_boot_t[composite_items, composites]
    composite_indicator_weights_p <- weights_boot_p[composite_items, composites]

    # If no composites
  } else {
    composite_indicator_reliability <- NA
    composite_indicator_weights_t <- NA
    composite_indicator_weights_p <- NA
  }

  # Clean boot matrices
  composite_indicator_weights_t[is.nan(composite_indicator_weights_t)] <- NA
  composite_indicator_weights_p[is.nan(composite_indicator_weights_p)] <- NA

  # Boot Measurement model
  message("Boot Measurement Model Evaluation:")

  # First report Factor metrics:
  message("\n------------------Factors:------------------")

  message("1. Indicator Reliability:\nLoadings:")
  message(factor_indicator_reliability)

  message("\n2. Factor Reliability and Convergent Validity:")
  message(factor_reliability)

  message("\n3. Discriminant Validity")
  message("HTMT")
  message(discriminant_validity)
  message("HTMT t-values:")
  message(discriminant_validity_t)
  message("HTMT p-values:")
  message(discriminant_validity_p)

  # Second report Composite metrics:
  message("\n------------------Composites:---------------")

  message("1. Indicator Reliability:\nWeights:")
  message(composite_indicator_reliability)
  message("\n Weights t-values:")
  message(composite_indicator_weights_t)
  message("\n Weights p-values:")
  message(composite_indicator_weights_p)
  message("\n2. Collinearity:\nItem VIFs per Construct:")
  message(metrics$validity$item_vifs[composites])

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
