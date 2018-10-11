# Simple Style: Seperate declaration of measurement,interactions and structural model.

library(seminr)

# Creating our measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  single_item("CUEX2")),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Interaction constructs must be created after the measurement model is defined.
# We are using the orthogonalization method as per Henseler & Chin (2010)
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation")
)

# Structural model
#  note: interactions should be the names of its main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation")
                )
)

# Load data, assemble model, and estimate using simplePLS
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         interactions = mobi_xm,
                         structural_model = mobi_sm)


