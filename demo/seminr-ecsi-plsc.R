# Simple Style: Seperate declaration of measurement and structural model, no interactions. Estimated
# using simplePLS consistent.
library(seminr)

# This example recreates the ECSI model on mobile users found at:
# https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
mobi <- mobi

# Creating measurement model
# - note, reflective() is used to specify common-factor reflective constructs
mobi_mm <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
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
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

summary(mobi_pls)
plot_scores(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

summary(boot_mobi_pls)
plot_scores(boot_mobi_pls)
