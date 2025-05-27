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
  # if (is.null(seminr_model$hoc)) {
  #   all_constructs <- intersect(unique(seminr_model$smMatrix),unique(seminr_model$mmMatrix[,1 ]))
  # } else {
  #   all_constructs <- intersect(unique(c(seminr_model$smMatrix, seminr_model$first_stage_model$smMatrix)),unique(seminr_model$mmMatrix[,1 ]))
  # }

  item_vifs <- sapply(model_constructs$construct_names, independent_vifs,
                      items_of_construct, seminr_model,
                      data = seminr_model$data)
  class(item_vifs) <- append(class(item_vifs), "list_output")
  item_vifs
}

# Calculate VIF of all antecedents of each construct
antecedent_vifs <- function(smMatrix, cor_matrix) {
  endogenous_names <- all_endogenous(smMatrix)
  ret <- sapply(endogenous_names, function(outcome) {
    antecedents <- antecedents_of(outcome, smMatrix)
    if (length(antecedents) == 1) {
      structure(NA, names=antecedents)
    } else {
      cor_vifs(cor_matrix, antecedents)
    }
  }, simplify=FALSE, USE.NAMES=TRUE)
  class(ret) <- append(class(ret), "list_output")
  ret
}

# HTMT as per Henseler, J., Ringle, C. M., & Sarstedt, M. (2014). A new criterion for assessing discriminant validity in
# variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115-135.
# https://doi.org/10.1007/s11747-014-0403-8
HTMT <- function(seminr_model) {
  if (is.null(seminr_model$hoc)) {
    constructs <- intersect(construct_names(seminr_model$smMatrix),unique(seminr_model$mmMatrix[,1 ]))
  } else {
    constructs <- intersect(unique(c(seminr_model$smMatrix[,1],seminr_model$smMatrix[,2], seminr_model$first_stage_model$smMatrix[,1],seminr_model$first_stage_model$smMatrix[,2])),unique(seminr_model$mmMatrix[,1 ]))
  }

  HTMT <- matrix(, nrow=length(constructs), ncol=length(constructs),
                 dimnames = list(constructs,constructs))
  for (constructi in constructs[1:(length(constructs)-1)]) {
    for (constructj in constructs[(which(constructs == constructi)+1):length(constructs)]) {
      manifesti <- seminr_model$mmMatrix[seminr_model$mmMatrix[, 1] == constructi, "measurement"]
      manifestj <- seminr_model$mmMatrix[seminr_model$mmMatrix[, 1] == constructj, "measurement"]
      item_correlation_matrix <- abs(stats::cor(seminr_model$data[, manifesti],seminr_model$data[, manifestj]))
      HTHM <- mean(item_correlation_matrix)
      if(length(manifesti)>1 ) {
        cor_matrix <- abs(stats::cor(seminr_model$data[, manifesti], seminr_model$data[, manifesti]))
        diag(cor_matrix) <- 0
        MTHM <- (2/(length(manifesti)*(length(manifesti)-1)))*(sum(cor_matrix[!lower.tri(cor_matrix)]))
      } else {
        MTHM <- 1
      }
      if(length(manifestj)>1) {
        cor_matrix2 <- abs(stats::cor(seminr_model$data[, manifestj], seminr_model$data[, manifestj]))
        diag(cor_matrix2) <- 0
        MTHM <- sqrt(MTHM * (2/(length(manifestj)*(length(manifestj)-1)))*(sum(cor_matrix2[!lower.tri(cor_matrix2)])))
      } else {
        MTHM <- sqrt(1 * MTHM)
      }
      HTMT[constructi, constructj] <- HTHM / MTHM
    }
  }
  convert_to_table_output(HTMT)
}

# fl_criteria_table can be used to generate simple and effective table for checking Fornell Larcker criteria.
# Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. Journal of marketing research, 18(1), 39-50.
fl_criteria_table <- function(seminr_model, model_constructs) {
  # if (is.null(seminr_model$hoc)) {
  #   construct_scores <- seminr_model$construct_scores
  # } else {
  #   constructs <- setdiff(unique(c(seminr_model$smMatrix, seminr_model$first_stage_model$smMatrix)),seminr_model$constructs)
  #   construct_scores <- cbind(seminr_model$construct_scores, seminr_model$first_stage_model$construct_scores[,constructs])
  # }
  table <- stats::cor(model_constructs$construct_scores)
  table[upper.tri(table)] <- NA
  diag(table) <- sqrt(rhoC_AVE(seminr_model, constructs = model_constructs$construct_names)[,"AVE"])
  comment(table) <- "FL Criteria table reports square root of AVE on the diagonal and construct correlations on the lower triangle."
  convert_to_table_output(table)
}
