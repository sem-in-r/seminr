# Purpose: Model-level accessors and selectors;
#          S3 methods dispatching on seminr_model

# -- S3 methods (model dispatch) -----------------------------

# Any estimated model (pls_model, cbsem_model, boot_seminr_model, etc.)
#' @export
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

# Estimated model: items for a construct (via mmMatrix)
#' @export
construct_items.seminr_model <- function(x, construct_name, ...) {
  construct_items(x$mmMatrix, construct_name)
}

# -- Accessors -----------------------------------------------

# Get user-facing measurement type string for a construct in a model
construct_type <- function(model, construct) {
  if (is_interaction(construct)) {
    return("interaction")
  }
  for (i in 1:length(model$measurement_model)) {
    cst <- model$measurement_model[[i]]
    # warning interaction are functions do not access their indexes
    if (!inherits(cst, "function")) {
      if (cst[[1]] == construct) {
        c_type <- cst[[3]]
      }
    }
  }

  return(c_type)
}

# Get construct scores from an estimated model (handles HOC first-stage merging)
construct_scores <- function(model) {
  if (is.null(model$hoc)) {
    model$construct_scores
  } else {
    first_stage_only <- setdiff(
      unique(model$first_stage_model$smMatrix),
      unique(model$smMatrix)
    )
    cbind(model$construct_scores,
          model$first_stage_model$construct_scores[, first_stage_only])
  }
}

# Bundle of construct names, types, and scores from a model
# Uses matrix-level accessors directly to work with unclassed model lists
# (called during estimation before class is assigned)
constructs_in_model <- function(model) {
  if (is.null(model$hoc)) {
    names <- intersect(construct_names(model$smMatrix), all_constructs(model$mmMatrix))
  } else {
    sm_constructs <- union(
      construct_names(model$smMatrix),
      construct_names(model$first_stage_model$smMatrix)
    )
    names <- intersect(sm_constructs, all_constructs(model$mmMatrix))
  }
  types <- sapply(names, function(n) construct_type(model, n), USE.NAMES = FALSE)
  scores <- construct_scores(model)
  list(construct_names = names,
       construct_types = types,
       construct_scores = scores)
}

return_only_composite_scores <- function(object){
  composite_modes <- c("A", "B", "HOCA", "HOCB", "UNIT")
  mm_composites <- unique(unlist(lapply(composite_modes, function(mode) all_constructs_of_mode(object$mmMatrix, mode))))
  used_composites <- intersect(mm_composites, object$constructs)
  if (length(used_composites) == 0) {
      return(NULL)
  } else {
    return(object$construct_scores[, used_composites])
  }
}

# -- Selectors -----------------------------------------------

all_factors <- function(seminr_model) {
  intersect(seminr_model$constructs, all_reflective(seminr_model$mmMatrix))
}

all_composites <- function(seminr_model) {
  setdiff(seminr_model$constructs, all_factors(seminr_model))
}
