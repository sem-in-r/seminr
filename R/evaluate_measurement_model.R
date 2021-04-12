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
  alpha <- cronbachs_alpha(seminr_model)
  mat1 <- rhoC_AVE(seminr_model)
  mat2 <- rho_A(seminr_model)
  table <- cbind(alpha, mat1, mat2)
  comment(table) <- "Alpha, rhoC, and rhoA should exceed 0.7 while AVE should exceed 0.5"
  class(table) <- append(class(table), c("table_output","reliability_table"))
  return(table)
}

# Validity ----
validity <- function(seminr_model) {
  list(
    htmt            = HTMT(seminr_model),
    cross_loadings  = cross_loadings(seminr_model),
    item_vifs       = item_vifs(seminr_model),
    # TODO: consider if antecedent vifs should be part of structural results
    antecedent_vifs = antecedent_vifs(seminr_model$smMatrix, stats::cor(seminr_model$construct_scores)),
    fl_criteria = fl_criteria_table(seminr_model)
  )
}

cross_loadings <- function(seminr_model) {
  ret <- stats::cor(seminr_model$data[, seminr_model$mmVariables], seminr_model$construct_scores)
  convert_to_table_output(ret)
}
