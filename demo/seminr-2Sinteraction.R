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
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Interaction constructs must be created after the measurement model is defined.
# We are using the two_stage method as per Henseler & Chin (2010)
mobi_xm <- interactions(
  interaction_2stage("Image", "Expectation")
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
                         interactions = mobi_xm,
                         structural_model = mobi_sm)
mobi_pls$path_coef
mobi_pls$rSquared

