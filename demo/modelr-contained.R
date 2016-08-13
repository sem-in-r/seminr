# Shows an example of modular vs. classic model design

source("R/syntax.R")
data("mobi")
# MODULAR style
# modelr syntax for creating measurement model

mobi_pls <- modelr(
  data = mobi,

  measure(
    reflect("Image", multi_items("IMAG", 1:5)),
    reflect("Expectation", multi_items("CUEX", 1:3)),
    reflect("Value", multi_items("PERV", 1:2)),
    reflect("Satisfaction", multi_items("CUSA", 1:3))
  ),

  interact(
    interaction_combo("Image", "Expectation"),
    interaction_combo("Image", "Value")
  ),

  structure(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value",
                   "Image.Expectation", "Image.Value"))
  )
)

mobi_pls_fitted <- sempls(model = mobi_pls$model, data = mobi_pls$data)
pathCoeff(mobi_pls_fitted)
rSquared(mobi_pls_fitted)
