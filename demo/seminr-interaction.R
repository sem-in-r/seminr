# Simple Style: Seperate declaration of measurement,interactions and structural model.

library(seminr)

# First, using the orthogonal method as per Henseler & Chin (2010).
# Creating our measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = c("Expectation"), method = orthogonal, weights = mode_A),
  interaction_term(iv = "Image", moderator = c("Value"), method = orthogonal, weights = mode_A)
)

# Structural model
#  note: interactions should be the names of its main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

summary(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

summary(boot_mobi_pls)

# Second, using the standardized product indicator method as per Henseler & Chin (2010).
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = c("Expectation"), method = product_indicator, weights = mode_A),
  interaction_term(iv = "Image", moderator = c("Value"), method = product_indicator, weights = mode_A)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

summary(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

summary(boot_mobi_pls)

library(seminr)

# Third, using the two_stage method as per Henseler & Chin (2010).
# Creating our measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = c("Expectation"), method = two_stage, weights = mode_A),
  interaction_term(iv = "Image", moderator = c("Value"), method = two_stage, weights = mode_A)
)

# Structural model
#  note: interactions should be the names of its main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

summary(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 500)

summary(boot_mobi_pls)
