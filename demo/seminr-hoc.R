# Simple Style: Seperate declaration of measurement and structural model, no interactions.
library(seminr)

# Creating measurement mode
# - note: composite() has a default parameter setting of mode A
# - note: items can be a list of names: c("CUEX1", "CUEX2", "CUEX3")
#         which can be constructed quickly as: multi_items("CUEX", 1:3)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  two_stage_hierarchical_construct("Satisfaction", c("Image","Value")),
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
                         interactions = NULL,
                         structural_model = mobi_sm)

