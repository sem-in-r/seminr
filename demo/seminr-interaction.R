# Simple Style: Seperate declaration of measurement,interactions and structural model.

# This example adapts on the ECSI model on mobile users found at:
#  https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
mobi <- mobi

library(seminr)

# Creating our measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Interaction factors must be created after the measurement model is defined.
# We are using the orthogonalization method as per Henseler & Chin (2010)
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# Structural model
#  note: interactions should be the names of its main constructs joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Load data, assemble model, and estimate using simplePLS
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         interactions = mobi_xm,
                         structural_model = mobi_sm)

print_paths(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

print_paths(boot_mobi_pls)

# Second, using the standardized product indicator method as per Henseler & Chin (2010).
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_scaled("Image", "Expectation"),
  interaction_scaled("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Load data, assemble model, and estimate using simplePLS
mobi <- mobi
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         interactions = mobi_xm,
                         structural_model = mobi_sm)

print_paths(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

print_paths(boot_mobi_pls)
