# Demonstration of a higher-order construct with lower order constructs
library(seminr)

# Currently only the two_stage solution is implemented.
# Reflective - Reflective type Higher Order Construct ----
# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  higher_composite("Satisfaction", dimensions = c("Image","Value"), method = two_stage, weights = mode_A),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = c("Expectation","Quality"),  to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

# Estimate the model with the HOC
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Plot the model
plot(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(mobi_pls, nboot = 1000)

# Pot the bootstrapped model
plot(boot_mobi_pls)


# Reflective - Formative type Higher Order Construct ----
# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_B),
  higher_composite("Satisfaction", dimensions = c("Image","Value"), method = two_stage, weights = mode_A),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = c("Expectation","Quality"),  to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

# Estimate the model with the HOC
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Plot the model
plot(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(mobi_pls, nboot = 1000)

# Pot the bootstrapped model
plot(boot_mobi_pls)

# Formative - Reflective type Higher Order Construct ----
# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  higher_composite("Satisfaction", dimensions = c("Image","Value"), method = two_stage, weights = mode_B),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = c("Expectation","Quality"),  to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

# Estimate the model with the HOC
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Plot the model
plot(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(mobi_pls, nboot = 1000)

# Pot the bootstrapped model
plot(boot_mobi_pls)

# Formative - Formative type Higher Order Construct ----
# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_B),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_B),
  higher_composite("Satisfaction", dimensions = c("Image","Value"), method = two_stage, weights = mode_B),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = c("Expectation","Quality"),  to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

# Estimate the model with the HOC
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

# Plot the model
plot(mobi_pls)

# Bootstrap the model
boot_mobi_pls <- bootstrap_model(mobi_pls, nboot = 1000)

# Pot the bootstrapped model
plot(boot_mobi_pls)
