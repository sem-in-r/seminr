# Creates summary statistics for a cbsem object for summary and print functions
summarize_cb_measurement <- function(object, alpha=0.05) {
  lavaan_output <- object$lavaan_output
  estimates <- lavaan::standardizedSolution(lavaan_output)

  model <- list(
    item_names       = all_items(object$measurement_model),
    construct_names  = all_construct_names(object$measurement_model),
    estimation       = lavaan_output@Model@estimator
  )

  # Get standardized parameter estimates (won't contain R^2)
  loadings_df <- estimates[estimates$op == "=~",]
  loadings_matrix <- df_xtab_matrix(est.std ~ rhs + lhs, loadings_df,
                                    model$item_names, model$construct_names)
  alpha_text <- alpha/2*100
  significance <- with(loadings_df,
                       data.frame(est.std, se, pvalue, ci.lower, ci.upper))
  rownames(significance) <- with(loadings_df, paste(lhs, "->", rhs))
  colnames(significance) <- c( "Std Estimate", "SE", "t-Value", paste(alpha_text, "% CI", sep = ""), paste((100-alpha_text), "% CI", sep = ""))

  # Get descriptives and correlations
  # item_descriptives <- desc(object$data)
  item_correlations <- stats::cor(object$data[, model$item_names])
  construct_correlations <- lavaan::lavInspect(lavaan_output, what = "cor.lv")

  list(
    meta = list(
      seminr = seminr_info(),
      engine = list(
        pkgname = "lavaan",
        version = lavaan_output@version,
        estimator = lavaan_output@Options$estimator
      ),
      syntax  = object$lavaan_model,
      call    = lavaan_output@call
    ),
    model = model,
    descriptives = list(
      # TODO: report item descriptive stats
      # statistics = list(
      #   items = item_descriptives
      # ),
      correlations = list(
        items = item_correlations,
        constructs = construct_correlations
      )
    ),
    loadings = list(
      coefficients = loadings_matrix,
      significance = significance
    )
  )
}

summarize_cb_structure <- function(object, alpha=0.05) {
  estimates <- lavaan::standardizedSolution(object$lavaan_output, level=1-alpha)

  # Capture structural relationship information
  all_antecedents <- all_exogenous(object$smMatrix)
  all_outcomes <- all_endogenous(object$smMatrix)

  # Path coefficients, p-values, R^2 for path matrix
  path_df <- estimates[estimates$op == "~",]
  rsq <- lavaan::inspect(object$lavaan_output, "r2")[all_outcomes]

  path_matrix     <- {
    df_xtab_matrix(est.std ~ rhs + lhs, path_df,
                   all_antecedents, all_outcomes) -> .
    rownames(.) <- all_antecedents
    rbind("R^2"=rsq, .)
  }

  pvalue_matrix <- {
    df_xtab_matrix(pvalue ~ rhs + lhs, path_df,
                   all_antecedents, all_outcomes) -> .
    rownames(.) <- all_antecedents
    colnames(.) <- all_outcomes
    .
  }

  alpha_text <- alpha/2*100
  significance <- with(path_df,
    data.frame(est.std, se, pvalue, ci.lower, ci.upper))

  rownames(significance) <- with(path_df, paste(lhs, "->", rhs))
  colnames(significance) <- c( "Std Estimate", "SE", "t-Value", paste(alpha_text, "% CI", sep = ""), paste((100-alpha_text), "% CI", sep = ""))

  # TODO v3: Remove pvalues from cbsem summary in lieu of significance table only
  list(
    coefficients = path_matrix,
    pvalues = pvalue_matrix,
    significance = significance
  )
}

# Returns selected fit metrics: ordinary, robustable, robust, and scaled
curated_fit_metrics <- function(fit_metrics) {
  metric_names <- names(fit_metrics)
  robust_names <- metric_names[grep("\\.robust", metric_names)]
  scaled_names <- metric_names[grep("\\.scaled", metric_names)]
  simple_names <- metric_names[grep("\\.|_", metric_names, invert=TRUE)]
  suffixed_names <- c(robust_names, scaled_names)

  robustable_names <- {
    regmatches(suffixed_names, regexpr("\\.(robust|scaled)", suffixed_names), invert = TRUE) -> .
    lapply(., FUN=function(x) { x[1] }) -> .
    unlist(.) -> .
    unique(.)
  }

  ordinary_names <- setdiff(simple_names, robustable_names)

  robust_metrics <- NULL
  if (!is.null(robustable_names)) {
    robust_metrics <- data.frame(
      metric = fit_metrics[robustable_names],
      scaled = fit_metrics[paste(robustable_names, ".scaled", sep="")],
      robust = fit_metrics[paste(robustable_names, ".robust", sep="")]
    )
  }

  list(
    ordinary = fit_metrics[ordinary_names],
    robust = robust_metrics
  )
}

summarize_fit <- function(lavaan_output) {
  lavaan_fit <- lavaan::fitMeasures(lavaan_output)

  list(
    all = lavaan_fit,
    curated = curated_fit_metrics(lavaan_fit)
  )
}
