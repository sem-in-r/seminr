# PURPOSE: functions for composite and factor validity

## VIF Functions ---------------------
# Generic: Gets VIF for all independent variables of a construct
independent_vifs <- function(construct, find_independents, seminr_model, data) {
  independents <- find_independents(construct, seminr_model)
  vifs <- if (length(independents) > 1)
    sapply(independents, compute_vif, independents, data)
  else structure(1, names = independents)
}

# Calculate VIF of all items of each construct
item_vifs <- function(seminr_model) {
  all_constructs <- seminr_model$constructs
  item_vifs <- sapply(all_constructs, independent_vifs,
                      items_of_construct, seminr_model,
                      data = seminr_model$data)
}

# Calculate VIF of all antecedents of each construct
antecedent_vifs <- function(seminr_model) {
  endogenous_constructs <- unique(seminr_model$smMatrix[,2])
  names(endogenous_constructs) <- endogenous_constructs # helps lapply return named list
  antecedent_vifs <- lapply(endogenous_constructs, independent_vifs,
                            antecedents_of_construct, seminr_model,
                            data = seminr_model$construct_scores)
}

# HTMT as per Henseler, J., Ringle, C. M., & Sarstedt, M. (2014). A new criterion for assessing discriminant validity in
# variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115-135.
# https://doi.org/10.1007/s11747-014-0403-8
HTMT <- function(seminr_model) {
  HTMT <- matrix(, nrow=length(seminr_model$constructs), ncol=length(seminr_model$constructs),
                 dimnames = list(seminr_model$constructs,seminr_model$constructs))
  for (constructi in seminr_model$constructs[1:(length(seminr_model$constructs)-1)]) {
    for (constructj in seminr_model$constructs[(which(seminr_model$constructs == constructi)+1):length(seminr_model$constructs)]) {
      manifesti <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == constructi]
      manifestj <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == constructj]
      item_correlation_matrix <- abs(stats::cor(seminr_model$data[,manifesti],seminr_model$data[,manifestj]))
      HTHM <- mean(item_correlation_matrix)
      if(length(manifesti)>1 ) {
        cor_matrix <- abs(stats::cor(seminr_model$data[,manifesti],seminr_model$data[,manifesti]))
        diag(cor_matrix) <- 0
        MTHM <- (2/(length(manifesti)*(length(manifesti)-1)))*(sum(cor_matrix[!lower.tri(cor_matrix)]))
      } else {
        MTHM <- 1
      }
      if(length(manifestj)>1) {
        cor_matrix2 <- abs(stats::cor(seminr_model$data[,manifestj],seminr_model$data[,manifestj]))
        diag(cor_matrix2) <- 0
        MTHM <- sqrt(MTHM * (2/(length(manifestj)*(length(manifestj)-1)))*(sum(cor_matrix2[!lower.tri(cor_matrix2)])))
      } else {
        MTHM <- sqrt(1 * MTHM)
      }
      HTMT[constructi,constructj] <- HTHM / MTHM
    }
  }
  return(HTMT)
}
