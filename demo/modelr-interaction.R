# Shows an example of adding interactions to a model

source("R/syntax.R")
data("mobi")

# modelr syntax for creating measurement model
mobi_mm <- measure(
  reflect("Image", multi_items("IMAG", 1:5)),
  reflect("Expectation", multi_items("CUEX", 1:3)),
  reflect("Value", multi_items("PERV", 1:2)),
  reflect("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_intxns <- interact(
  data = mobi, mm = mobi_mm,
  interaction_combo("Image", "Expectation"),
  interaction_combo("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- structure(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Regular semPLS functions to create andestimate model, and report estimates
mobi_pls <- modelr(mobi, mobi_mm, mobi_sm, mobi_intxns)
mobi_pls_fitted <- sempls(model = mobi_pls$model, data = mobi_pls$data)
pathCoeff(mobi_pls_fitted)
rSquared(mobi_pls_fitted)
