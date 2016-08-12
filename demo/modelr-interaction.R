# Shows an example of adding interactions to a model

source("R/syntax.R")
data("mobi")

# modelr syntax for creating measurement model

mobi_mm <- measure(
  reflect("Image", "IMAG", 1:5),
  reflect("Expectation", "CUEX", 1:3),
  reflect("Quality", "PERQ", 1:7)
)

# interactions must be created after the measurement model is defined
mobi_intxns <- interact(data = mobi, mm = mobi_mm,
                        interaction_combo("Image", "Expectation")
)

# modelr syntax for creating structural model
# - note, three ways to represent the structure

mobi_sm <- structure(
  paths(to = "Quality", from = c("Image", "Expectation"))
)

# Regular semPLS functions to create andestimate model, and report estimates
mobi_pls <- plsm(data = mobi, strucmod = mobi_sm, measuremod = mobi_mm)
mobi_pls_fitted <- sempls(model = mobi_pls, data = mobi)
pathCoeff(mobi_pls_fitted)
rSquared(mobi_pls_fitted)

assemble_model <- function(data, measurement_model, structural_model, interactions=NULL) {
  if(!is.null(interactions)) {
    get_data <- function(intxn) { intxn$data }
    interaction_data <- do.call("cbind", lapply(interactions, get_data))
    data <- cbind(data, interaction_data)
  }

  return(plsm(data = data, strucmod = structural_model, measuremod = measurement_model))
}

