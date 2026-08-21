# PURPOSE: functions for composite and factor validity

## VIF Functions ---------------------
# Generic: Gets VIF for all independent variables of a construct
independent_vifs <- function(construct, find_independents, seminr_model, data) {
  independents <- find_independents(construct, seminr_model)
  # TODO: remove dependence on compute_vif and use cor_vifs instead
  vifs <- if (length(independents) > 1)
    sapply(independents, compute_vif, independents, data)
  else structure(1, names = independents)
}

# Calculate VIF of all items of each construct
item_vifs <- function(seminr_model, model_constructs) {
  item_vifs <- sapply(model_constructs$construct_names, independent_vifs,
                      function(construct, model) construct_items(model, construct), seminr_model,
                      data = seminr_model$data,
                      simplify = FALSE)
  class(item_vifs) <- append(class(item_vifs), "list_output")
  item_vifs
}

# Calculate VIF of all antecedents of each construct
antecedent_vifs <- function(smMatrix, cor_matrix) {
  endogenous_names <- all_endogenous(smMatrix)
  ret <- sapply(endogenous_names, function(outcome) {
    antecedents <- construct_antecedents(smMatrix, outcome)
    if (length(antecedents) == 1) {
      structure(NA, names = antecedents)
    } else {
      cor_vifs(cor_matrix, antecedents)
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
  class(ret) <- append(class(ret), "list_output")
  ret
}

# HTMT as per Henseler, J., Ringle, C. M., & Sarstedt, M. (2014). A new criterion for assessing discriminant validity in
# variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115-135.
# https://doi.org/10.1007/s11747-014-0403-8
HTMT <- function(seminr_model) {
  if (is.null(seminr_model$hoc)) {
    constructs <- intersect(construct_names(seminr_model$smMatrix), all_constructs(seminr_model$mmMatrix))
  } else {
    constructs <- intersect(unique(c(construct_names(seminr_model$smMatrix), construct_names(seminr_model$first_stage_model$smMatrix))), all_constructs(seminr_model$mmMatrix))
  }

  HTMT <- matrix(, nrow=length(constructs), ncol=length(constructs),
                 dimnames = list(constructs,constructs))

  # Hoist the data.frame->matrix conversion and dedupe within-construct
  # correlation blocks (the original recomputed them for every pair). The
  # two-argument stats::cor() form is kept so the arithmetic stays identical.
  item_lists <- sapply(constructs, function(c) construct_items(seminr_model$mmMatrix, c),
                       simplify = FALSE)
  all_manifests <- unique(unlist(item_lists, use.names = FALSE))
  data_matrix <- as.matrix(seminr_model$data[, all_manifests])

  # Within-construct correlation terms, computed once per construct. The
  # scaling factor and correlation sum are kept separate so the per-pair
  # multiplication below reproduces the original expression's left-to-right
  # grouping exactly (floating-point multiplication is not associative).
  within_stats <- lapply(item_lists, function(manifests) {
    if (length(manifests) > 1) {
      cor_matrix <- abs(stats::cor(data_matrix[, manifests], data_matrix[, manifests]))
      diag(cor_matrix) <- 0
      cor_factor <- 2/(length(manifests)*(length(manifests)-1))
      cor_sum <- sum(cor_matrix[!lower.tri(cor_matrix)])
      list(mean_cor = cor_factor*(cor_sum), factor = cor_factor, sum = cor_sum)
    } else {
      NULL
    }
  })

  for (constructi in constructs[1:(length(constructs)-1)]) {
    for (constructj in constructs[(which(constructs == constructi)+1):length(constructs)]) {
      manifesti <- item_lists[[constructi]]
      manifestj <- item_lists[[constructj]]
      item_correlation_matrix <- abs(stats::cor(data_matrix[, manifesti], data_matrix[, manifestj]))
      HTHM <- mean(item_correlation_matrix)
      within_i <- within_stats[[constructi]]
      within_j <- within_stats[[constructj]]
      MTHM <- if (is.null(within_i)) 1 else within_i$mean_cor
      MTHM <- if (is.null(within_j)) {
        sqrt(1 * MTHM)
      } else {
        sqrt(MTHM * within_j$factor * within_j$sum)
      }
      HTMT[constructi, constructj] <- HTHM / MTHM
    }
  }
  convert_to_table_output(HTMT)
}

# fl_criteria_table can be used to generate simple and effective table for checking Fornell Larcker criteria.
# Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. Journal of marketing research, 18(1), 39-50.
fl_criteria_table <- function(seminr_model, model_constructs) {
  table <- stats::cor(model_constructs$construct_scores)
  table[upper.tri(table)] <- NA
  diag(table) <- sqrt(rhoC_AVE(seminr_model, constructs = model_constructs$construct_names)[,"AVE"])
  comment(table) <- "FL Criteria table reports square root of AVE on the diagonal and construct correlations on the lower triangle."
  convert_to_table_output(table)
}
