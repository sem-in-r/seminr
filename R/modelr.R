# Model Assembly Functions
modelr <- function(data, measurement_model, interactions=NULL, structural_model, ...) {
  if(!is.null(interactions)) {
    # update data with new iteraction items
    intxns_list <- interactions(data, measurement_model)
    get_data <- function(intxn) { intxn$data }
    interaction_data <- do.call("cbind", lapply(intxns_list, get_data))
    data <- cbind(data, interaction_data)

    # update measurement model with
    measure_interaction <- function(intxn) {
      reflect(intxn$name, names(intxn$data))
    }
    intxns_mm <- measure(do.call("c", lapply(intxns_list, measure_interaction)))
    measurement_model <- rbind(measurement_model, intxns_mm)
  }

  model = semPLS::plsm(data = data, strucmod = structural_model, measuremod = measurement_model)
  cat("Estimating model using semPLS::sempls...\n")
  mobi_pls_fitted <- semPLS::sempls(model, data, ...)
}
