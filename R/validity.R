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
