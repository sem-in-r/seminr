# This example recreates the ECSI model on mobile users found at:
# https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf

library(seminr)
# Simple Style: Seperate declaration of measurement and structural model, with interactions. Estimated
# using simplePLS consistent.

# seminr syntax for creating measurement mode
# - note, The antecedent constructs Image and Expectation will later have an
# interaction term created, but not specified at this point. Satisfaction is the
# outcome construct.
mobi_mm <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = "Satisfaction"),
  paths(from = "Expectation",  to = "Satisfaction")
)

# Regular semPLS functions to create and estimate model without interaction.
# - note, interaction is only calculated in the second stage.
mobi <- mobi

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

print_paths(mobi_pls)

# Implement the interaction of the antecedent constructs
mobi_plsc_interact <- PLSc_interact(mobi_pls)

# path coefficients, R^2 and loadings
mobi_plsc_interact$path_coef
mobi_plsc_interact$rSquared
mobi_plsc_interact$outer_loadings
