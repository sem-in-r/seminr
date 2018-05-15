# PURPOSE: functions for composite and factor validity

# Gets VIF for independent variables of a construct
independent_vifs <- function(construct, find_independents, data) {
  independents <- find_independents(construct, seminr_model)
  vifs <- if (length(independents) > 1)
    sapply(independents, compute_vif, independents, data)
  else structure(1, names = independents)
}

# Calculate VIF of items of each construct
item_vifs <- function(seminr_model) {
  all_constructs <- seminr_model$ltVariables
  item_vifs <- sapply(all_constructs, independent_vifs,
                      items_of_construct, data = seminr_model$data)
}

# Calculate VIF of antecedents of each construct
antecedent_vifs <- function(seminr_model) {
  endogenous_constructs <- unique(seminr_model$smMatrix[,2])
  antecedent_vifs <- sapply(endogenous_constructs, independent_vifs,
                            antecedents_of_construct, data = seminr_model$construct_scores)
}
