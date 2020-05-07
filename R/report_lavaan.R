summarize_cb_measurement <- function(object) {
  lavaan_model <- object$lavaan_model
  estimates <- standardizedSolution(lavaan_model)

  # TODO: extract names from seminr rather than lavaan
  model <- list(
    item_names       = all_items(object$measurement_model),
    construct_names  = all_construct_names(object$measurement_model),
    estimation       = lavaan_model@Model@estimator
  )

  # Get standardized parameter estimates (won't contain R^2)
  loadings_df <- estimates[estimates$op == "=~",]
  loadings_matrix <- df_xtab_matrix(est.std ~ rhs + lhs, loadings_df,
                                    model$item_names, model$construct_names)

  # Get descriptives and correlations
  # item_descriptives <- desc(object$data)
  item_correlations <- stats::cor(object$data)
  construct_correlations <- lavInspect(lavaan_model, what = "cor.lv")

  # Scores, weights
  tenB <- estimate_lavaan_ten_berge(lavaan_model)

  list(
    meta = list(
      seminr = seminr_info(),
      engine = list(
        pkgname = "lavaan",
        version = lavaan_model@version,
        estimator = lavaan_model@Options$estimator
      ),
      syntax  = object$lavaan_syntax,
      call    = lavaan_model@call
    ),
    model = model,
    descriptives = list(
      # statistics = list(
      #   items = item_descriptives
      #   # TODO: report construct descriptive stats
      # ),
      correlations = list(
        items = item_correlations,
        constructs = construct_correlations
      )
    ),
    loadings = loadings_matrix,
    factor_scores = tenB$scores,
    weights = tenB$weights
  )
}

summarize_cb_structure <- function(object) {
  estimates <- standardizedSolution(object$lavaan_model)

  # Capture structural relationship information
  all_antecedents <- all_exogenous(object$smMatrix)
  all_outcomes <- all_endogenous(object$smMatrix)

  # Get R^2 results (only found in unstandandardized results?)
  invisible(capture.output(
    lav_summary <- lavaan::summary(object$lavaan_model, rsquare=TRUE)
  ))
  rsq_rows <- lav_summary$PE[(lav_summary$PE[,"lhs"] %in% all_outcomes) &
                               (lav_summary$PE[,"op"] == "r2"), ]
  rsq <- {
    rsq_rows[, "est"] -> .
    names(.) <- rsq_rows[, "lhs"]
    .[all_outcomes] # get correct order
  }

  # Path coefficients and p-values
  path_df <- estimates[estimates$op == "~",]

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

  list(
    coefficients = path_matrix,
    pvalues = pvalue_matrix
  )
}

#' Returns selected fit metrics: ordinary, robustable, robust, and scaled
curated_fit_metrics <- function(fit_metrics) {
  metric_names <- names(fit_metrics)
  robust_names <- metric_names[grep("\\.robust", metric_names)]
  scaled_names <- metric_names[grep("\\.scaled", metric_names)]
  simple_names <- metric_names[grep("\\.|_", metric_names, invert=TRUE)]
  suffixed_names <- c(robust_names, scaled_names)

  robustable_names <- {
    regmatches(suffixed_names, regexpr("\\.(robust|scaled)", suffixed_names), invert = TRUE) -> .
    lapply(., FUN=function(x) { x[1] }) -> .
    # TODO: only select prefixes with simple names (no dots in them)
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

summarize_fit <- function(lavaan_model) {
  lavaan_fit <- fitMeasures(lavaan_model)

  list(
    all = lavaan_fit,
    curated = curated_fit_metrics(lavaan_fit)
  )
}
