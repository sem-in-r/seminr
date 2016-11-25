# Shows an example of adding interactions to a model
library(seminr)

# seminr syntax for creating measurement model
mobi_mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Expectation",  multi_items("CUEX", 1:3)),
  reflect("Value",        multi_items("PERV", 1:2)),
  reflect("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interact(
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

# Load data, assemble model, and estimate using semPLS
data("mobi", package = "semPLS")
seminr_model <- seminr::create_model(data = mobi,
                                     measurement_model = mobi_mm,
                                     interaction = mobi_xm,
                                     structural_model = mobi_sm)

mobi_pls <- seminr::estimate_model(seminr_model)
print_paths(mobi_pls)
