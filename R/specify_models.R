#' @export
specify_model <- function(measurement_model, structural_model=NULL, item_associations=NULL) {
  specified_model <- list(
    measurement_model = measurement_model,
    structural_model  = structural_model,
    item_associations = item_associations
  )

  class(specified_model) <- append(class(specified_model), c("specified_model", "seminr_model"))
  specified_model
}

extract_models <- function(model = NULL, measurement_model = NULL, structural_model = NULL, item_associations = NULL) {
  if (is(model, "specified_model")) {
    if (is.null(measurement_model)) { measurement_model <- model$measurement_model }
    if (is.null(structural_model)) { structural_model <- model$structural_model }
    if (is.null(item_associations)) { item_associations <- model$item_associations }
  }

  list(measurement_model = measurement_model,
       structural_model = structural_model,
       item_associations = item_associations)
}
