library(seminr)

# Load your data, here we just use the mobi dataset that comes with seminr
mobi <- mobi

# Create the measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Create the structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Estimate the model on the full data sample
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm,
                         missing = mean_replacement,
                         missing_value = NA)

# Define two subgroups on the data based on any boolean criteria
sum( mobi$CUEX1 < 8  ) # 108 rows
sum( mobi$CUEX1 >= 8 ) # 142 rows

# Process PLS-MGA on estimated model and given criteria
mobi_mga <- estimate_pls_mga(mobi_pls, mobi$CUEX1 < 8)
