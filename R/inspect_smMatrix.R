# Purpose: inspect a structural model/matrix

# S3 generic: get construct names from various model objects
construct_names <- function(x, ...) {
  UseMethod("construct_names")
}

# Structural model matrix (smMatrix)
construct_names.structural_model <- function(x, ...) {
  unique(c(x[, "source"], x[, "target"]))
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

# mmMatrix: unique construct names
construct_names.mmMatrix <- function(x, ...) {
  all_constructs(x)
}

# Default fallback for unclassed matrices (smMatrix or mmMatrix after class stripping)
construct_names.default <- function(x, ...) {
  if ("construct" %in% colnames(x)) {
    unique(x[, "construct"])
  } else {
    unique(c(x[, "source"], x[, "target"]))
  }
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
  setdiff(unique(smMatrix[, "source"]), unique(smMatrix[, "target"]))
}

# Get constructs that are only targets (never a source)
only_endogenous <- function(smMatrix) {
  setdiff(all_endogenous(smMatrix), all_exogenous(smMatrix))
}

# Get antecedent construct names for a given target construct
construct_antecedents <- function(smMatrix, outcome) {
  smMatrix[smMatrix[, "target"] == outcome, "source"]
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

# Check if construct names in structural model are valid with respect to measurement model
are_construct_names_valid <- function(measurement_model,
                                  structural_model) {
  # remove interactions from the list (not created yet)
  sm_constructs <- construct_names(structural_model)
  sm_constructs <- sm_constructs[!is_interaction(sm_constructs)]
  mm_constructs <- construct_names(measurement_model)

  # construct names in sm DO occur in mm and are spelled correct
  construct_named_correcty <- all(sm_constructs %in% mm_constructs)

  # construct names do not occur in the indicator names
  construct_item_named_same <- any(sm_constructs %in% mm_constructs)

  return(!construct_named_correcty | !construct_item_named_same)
}

# Test if interaction terms are missing their direct effects in smMatrix
has_direct_effects <- function(smMatrix) {
  log_vec <- c(FALSE)
  if (has_interactions(smMatrix)) {
    ints <- all_interactions(smMatrix)
    for(con in ints) {
      outcomes <- construct_targets(smMatrix, con)
      for (outs in outcomes) {
        ants <- construct_antecedents(smMatrix, outs)
        end_lv_one <- regexpr("\\*", con)[1]
        lv_one <- substring(con, 0, end_lv_one - 1)
        lv_two <- substring(con, end_lv_one + 1, nchar(con))
        output <- !all(c(lv_one, lv_two) %in% ants)
        log_vec <- c(log_vec, output)
      }
    }
  }
  return(any(log_vec))
}

# Test if any paths in smMatrix target a given construct
has_paths_to <- function(smMatrix, target) {
  any(smMatrix[, "target"] == target)
}

# --- smMatrix computed accessors ---

# Function to subset a smMatrix by construct — return targets for a given source
subset_by_construct <- function(x, smMatrix) {
  construct_targets(smMatrix, x)
}

# Function to check whether a named construct's antecedents occur in a list
construct_antecedent_in_list <- function(x, list, smMatrix) {
  all(construct_antecedents(smMatrix, x) %in% list)
}

# Iterate over a vector of constructs and return all unique targets they depend on
construct_antecedents_all <- function(constructs_vector, smMatrix) {
  return(unique(unlist(sapply(constructs_vector, subset_by_construct, smMatrix = smMatrix), use.names = FALSE)))
}

# Iterate over a vector of constructs and check whether each construct's antecedents occur in a list
have_antecedents_in <- function(constructs_vector, list, smMatrix) {
  as.logical(sapply(constructs_vector, construct_antecedent_in_list, list = list, smMatrix = smMatrix))
}

# Organize order of endogenous constructs from most exogenous forwards
construct_order <- function(smMatrix) {

  # get purely endogenous and purely exogenous
  only_endo <- only_endogenous(smMatrix)
  only_exo <- only_exogenous(smMatrix)

  # get construct names
  all_constructs <- construct_names(smMatrix)

  # get all exogenous constructs
  all_exogenous_constructs <- setdiff(all_constructs, only_endo)

  # initialize construct order with first purely exogenous construct
  construct_order <- only_exo

  # Iterate over constructs to generate construct_order
  while (!setequal(all_exogenous_constructs, construct_order)) {
    construct_order <- c(construct_order, setdiff(construct_antecedents_all(construct_order, smMatrix)[have_antecedents_in(construct_antecedents_all(construct_order, smMatrix), construct_order, smMatrix)], construct_order))
  }

  # return the order of endogenous constructs to be predicted
  final_list <- setdiff(construct_order, only_exo)
  return(c(final_list, only_endo))

}

# --- smMatrix mutators ---

# Remove all paths targeting a given construct (or constructs)
remove_paths_to <- function(smMatrix, target) {
  smMatrix[!(smMatrix[, "target"] %in% target), , drop = FALSE]
}

# Remove all paths from given source construct(s)
remove_paths_from <- function(smMatrix, source) {
  smMatrix[!(smMatrix[, "source"] %in% source), , drop = FALSE]
}

# Keep only paths from sources in the given set
keep_paths_from <- function(smMatrix, sources) {
  smMatrix[smMatrix[, "source"] %in% sources, , drop = FALSE]
}

# Remove a specific source->target path
remove_path <- function(smMatrix, source, target) {
  keep <- !(smMatrix[, "source"] == source &
              smMatrix[, "target"] == target)
  smMatrix[keep, , drop = FALSE]
}
