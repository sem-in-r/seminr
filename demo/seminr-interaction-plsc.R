# This example recreates the ECSI model on mobile users found at:
# https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf

library(seminr)
# Simple Style: Seperate declaration of measurement and structural model, with interactions. Estimated
# using simplePLS consistent.

# Creating measurement model of main constructs
mobi_mm <- constructs(
  composite("Image",       multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value", multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Defining our interaction terms:
mobi_intxn <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# Creating structural model
#  note: interactions are named with their two main constructs with a dot '.' between (e.g., "Image.Expectation")
mobi_sm <- relationships(
  paths(from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"),
        to = "Satisfaction")
)


# Regular semPLS functions to create and estimate model without interaction.
# - note, interaction is only calculated in the second stage.
mobi <- mobi

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm,
                         interactions = mobi_intxn)

summary(mobi_pls)
