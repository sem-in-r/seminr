# Purpose: Model-level accessors, predicates, and selectors;
#          S3 methods dispatching on seminr_model
#
# Naming conventions used in this file:
#   Category    | Pattern            | Example
#   S3 method   | generic.class      | construct_names.seminr_model(x)
#   Accessor    | object_qualifier   | construct_type(model, name)
#   Predicate   | is_/has_/are_      | has_reflective(model)
#   Selector    | all_noun           | all_factors(model), all_composites(model)
#
# All functions use container-first argument order: model as the
# first argument.
#
# See also: helpers-mmMatrix.R, helpers-smMatrix.R

# -- S3 methods (generic.seminr_model: model dispatch) --------

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

# -- Accessors (object_qualifier: return single value) ---------

#' Get the user-facing measurement type of a construct
#'
#' Returns the measurement type string (e.g., `"composite"`, `"reflective"`,
#' `"interaction"`) for a construct in an estimated model.
#'
#' @param model An estimated seminr model.
#' @param construct The construct name (or construct specification, for
#'   interaction constructs).
#'
#' @return A character string identifying the construct type.
#' @export
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

# -- Predicates (is_/has_/are_: return logical) ----------------

# Does the model have reflective constructs? estimate_pls() runs PLSc
# (model_consistent) exactly when this is TRUE, so prediction uses it too.
# Only constructs in the model count; a higher-order composite of common
# factors counts too, because PLSc corrects it (hoc_composite_reliability())
has_reflective <- function(seminr_model) {
  if (length(all_factors(seminr_model)) > 0) return(TRUE)
  first_stage <- seminr_model$first_stage_model
  if (is.null(first_stage)) return(FALSE)
  hocs <- higher_order_composites(seminr_model, seminr_model$constructs)
  any(vapply(hocs, function(hoc) {
    any(construct_items(seminr_model$mmMatrix, hoc) %in% all_factors(first_stage))
  }, logical(1)))
}

# Does the model contain interaction terms? True when the measurement model
# specifies an interaction_term(): the test estimate_pls() uses to set
# $interaction, a field that is set only after PLSc has run and that CB-SEM
# models never have. Reads the class of each measurement model element, not its
# name, because an interaction appended to a measurement model can be unnamed,
# and not the structural model, because a CB-SEM one spells "A*B" as "A_x_B".
# A plain function, not an S3 method: model_consistent() calls it on the list
# simplePLS() returns, which does not have the "seminr_model" class yet
has_interaction_terms <- function(seminr_model) {
  length(all_interaction_fns(seminr_model$measurement_model)) > 0
}

# -- Selectors (all_noun: return vectors) ----------------------

#' Get all common-factor (reflective) constructs in a model
#'
#' Returns the names of constructs estimated as common factors (reflective
#' measurement) in an estimated seminr model.
#'
#' @param seminr_model An estimated seminr model.
#'
#' @return A character vector of construct names.
#' @export
all_factors <- function(seminr_model) {
  intersect(seminr_model$constructs, all_reflective(seminr_model$mmMatrix))
}

# Get the higher-order composites among the given constructs of a stage-2 model:
# those that are not common factors or interactions and whose indicators are all
# construct scores of the first-stage model (two-stage approach)
#
# @param seminr_model  The stage-2 seminr_model, with first_stage_model attached
# @param constructs    Constructs to search
# @return Names of the constructs whose indicators are LOC scores
higher_order_composites <- function(seminr_model, constructs) {
  first_stage <- seminr_model$first_stage_model
  if (is.null(first_stage)) return(character(0))
  is_hoc <- vapply(constructs, function(construct) {
    !(construct %in% all_factors(seminr_model)) && !is_interaction(construct) &&
      all(construct_items(seminr_model$mmMatrix, construct) %in% first_stage$constructs)
  }, logical(1))
  constructs[is_hoc]
}

#' Get all composite constructs in a model
#'
#' Returns the names of constructs estimated as composites (i.e., not
#' reflective common factors) in an estimated seminr model.
#'
#' @param seminr_model An estimated seminr model.
#'
#' @return A character vector of construct names.
#' @export
all_composites <- function(seminr_model) {
  setdiff(seminr_model$constructs, all_factors(seminr_model))
}

# Get the model's indicator names without the interaction items (those with a
# "*"), in mmVariables order. Reads $mmVariables directly, so it also works on
# the list simplePLS() returns while estimate_pls() is still running
all_non_interaction_items <- function(seminr_model) {
  seminr_model$mmVariables[!is_interaction(seminr_model$mmVariables)]
}
