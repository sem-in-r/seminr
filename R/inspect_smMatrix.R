# Purpose: inspect a structural model/matrix

# S3 generic: get construct names from various model objects
construct_names <- function(x, ...) {
  UseMethod("construct_names")
}

# Structural model matrix (smMatrix)
construct_names.structural_model <- function(x, ...) {
  unique(c(x[,1], x[,2]))
}

# Any estimated model (pls_model, cbsem_model, boot_seminr_model, etc.)
construct_names.seminr_model <- function(x, ...) {
  if (is.null(x$hoc)) {
    intersect(construct_names(x$smMatrix), all_constructs(x$mmMatrix))
  } else {
    sm_constructs <- union(
      construct_names(x$smMatrix),
      construct_names(x$first_stage_model$smMatrix)
    )
    intersect(sm_constructs, all_constructs(x$mmMatrix))
  }
}

# Measurement model list
construct_names.measurement_model <- function(x, ...) {
  constructs_only <- all_non_interactions(x)
  lapply(constructs_only, FUN=construct_name) -> .
  unlist(., use.names = FALSE)
}

# List fallback (measurement_model after append() loses class)
construct_names.list <- function(x, ...) {
  constructs_only <- all_non_interactions(x)
  lapply(constructs_only, FUN=construct_name) -> .
  unlist(., use.names = FALSE)
}

# Default fallback for unclassed matrices
construct_names.default <- function(x, ...) {
  unique(c(x[,1], x[,2]))
}

# Get all endogenous construct names in a structural model
all_endogenous <- function(smMatrix) {
  unique(smMatrix[, "target"])
}

# Get all exogenous construct names in a structural model
all_exogenous <- function(smMatrix) {
  unique(smMatrix[, "source"])
}

only_exogenous <- function(smMatrix) {
  setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))
}

# Get constructs that are only targets (never a source)
only_endogenous <- function(smMatrix) {
  setdiff(all_endogenous(smMatrix), all_exogenous(smMatrix))
}

# Get antecedent construct names for a given target construct
construct_antecedents <- function(smMatrix, outcome) {
  smMatrix[smMatrix[,2] == outcome, "source"]
}

# Get target construct names for a given source construct
construct_targets <- function(smMatrix, source) {
  smMatrix[smMatrix[, "source"] == source, "target"]
}

# Identify the antecedents of a dv that are interactions
construct_interactions <- function(smMatrix, outcome) {
  ants <- construct_antecedents(smMatrix, outcome)
  ants[grep("\\*", ants)]
}

# Identify if interactions occur in the sm model
all_interactions <- function(smMatrix) {
  construct_names(smMatrix)[grep("\\*",construct_names(smMatrix))]
}

# Test if a construct name is an interaction term (contains "*")
is_interaction <- function(construct_name) {
  grepl("\\*", construct_name)
}

# Test if smMatrix (or a specific DV's antecedents) includes interaction terms
has_interactions <- function(smMatrix, outcome = NULL) {
  if (is.null(outcome)) {
    any(is_interaction(construct_names(smMatrix)))
  } else {
    any(is_interaction(construct_antecedents(smMatrix, outcome)))
  }
}
