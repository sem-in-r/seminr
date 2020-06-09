# Purpose: inspect a structural model/matrix

# Get all unique construct names in a structural model
construct_names <- function(smMatrix) {
  unique(c(smMatrix[,1], smMatrix[,2]))
}

# Get all endogenous construct names in a structural model
all_endogenous <- function(smMatrix) {
  unique(smMatrix[, "target"])
}

# Get all exogenous construct names in a structural model
all_exogenous <- function(smMatrix) {
  unique(smMatrix[, "source"])
}

# Get antecedent construct names for a give construct in a model
antecedents_of <- function(outcome, smMatrix) {
  smMatrix[smMatrix[,2] == outcome, "source"]
}
