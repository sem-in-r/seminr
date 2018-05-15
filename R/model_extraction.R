# PURPOSE: functions to extract elements of estimated seminr models (seminr_model)

# Gets item names for a given construct in a model
items_of_construct <- function(construct, model) {
  model$mmMatrix[model$mmMatrix[,1] == construct, 2]
}

# Get antecedent construct names for a give construct in a model
antecedents_of_construct <- function(construct, model) {
  model$smMatrix[model$smMatrix[,2] == construct, 1]
}
