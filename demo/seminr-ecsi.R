# This example recreates the ECSI model on mobile users found at:
#  https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf

library(seminr)
# Simple Style: Seperate declaration of measurement and structural model, no interactions. Estimated
# using simplePLS.

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "regression"),
  composite("Quality",      multi_items("PERQ", 1:7),weights = "B"),
  composite("Value",        multi_items("PERV", 1:2),weights = "correlation"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A"),
  reflective("Complaints",  single_item("CUSCO")),
  reflective("Loyalty",     multi_items("CUSL", 1:3))
)

# seminr syntax for creating structural model
# - note, three ways to represent the structural relationships
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Regular semPLS functions to create and estimate model, and report estimates
mobi <- mobi

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

print_paths(mobi_pls)
plot_scores(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

print_paths(boot_mobi_pls)
plot_scores(boot_mobi_pls)
