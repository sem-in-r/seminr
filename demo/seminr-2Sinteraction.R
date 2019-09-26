# Simple Style: Seperate declaration of measurement and structural model, no interactions.
library(seminr)

# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term("Image*Expectation", dimensions = c("Image","Expectation"), method = two_stage, weights = mode_A)
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation"))
)

# Estimate the model with the HOC
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)
