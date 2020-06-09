# Demonstration of specifying and estimating covariance-based models
# - Confirmatory Factor Analysis (CFA) conducted  to confirm measurement model
# - Full structural equation model (CBSEM) conducted to confirm structural model

library(seminr)

# Get data from file or elsewhere.
# For this demo, we will use the included mobi dataset.
mobi <- mobi

# Creating measurement mode
# - items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#   which can be constructed quickly as: multi_items("CUEX", 1:3)
# - interactions between two constructs should be defined as a measurement term
mobi_mm <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  reflective("Expectation", multi_items("CUEX", 1:3)),
  reflective("Loyalty",     multi_items("CUSL", 1:3)),
  reflective("Value",       multi_items("PERV", 1:2)),
  reflective("Complaints",  single_item("CUSCO"))
)

# Identify any inter-item association parameters to estimate by
# specifying free associations between their errors
mobi_am <- associations(
  item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
  item_errors("IMAG1", "CUEX2")
)

# CONFIRMATORY FACTOR ANALYSIS
mobi_cfa <- estimate_cfa(mobi, mobi_mm, mobi_am)
summary(mobi_cfa)

# STRUCTURAL EQUATION MODEL

# First, let's append an interaction onto the measurement model
# - by default, a two-stage approach will be used to create a single item
#   interaction from CFA construct scores (ten Berge extraction)
# - we will specify a product indicator interaction method instead
final_mm <- append(
  mobi_mm,
  interaction_term("Image", "Expectation", method = product_indicator)
)

# Specify the structural model
# - we can create multiple paths from a single `paths()` function
# - Six structural paths are created quickly in two lines!
mobi_sm <- relationships(
  paths(from = c("Image", "Expectation"), to = c("Value", "Loyalty")),
  paths(from = c("Complaints", "Image*Expectation"), to = "Loyalty")
)


# Estimate the SEM and get results
# - if the measurement model contains composites, use `all.reflective(final_mm)` 
#   to convert all constructs to reflective measurement
mobi_cbsem <- estimate_cbsem(mobi, final_mm, mobi_sm, mobi_am)
summary(mobi_cbsem)

# Examine other interesting results by inspecting the summary object
cbsem_summary <- summary(mobi_cbsem)
# - factor loadings
cbsem_summary$loadings
# - latent variable correlations
cbsem_summary$descriptives$correlations$constructs
# - Check the Variance Inflation Factor (VIF) of each regression
cbsem_summary$quality$antecedent_vifs