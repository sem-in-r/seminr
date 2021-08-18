### Accompanying Code for:
## Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
## Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.

## Chapter 4: Evaluation of reflective measurement models

# Load the SEMinR library
library(seminr)

# Load the data ----
corp_rep_data <- corp_rep_data

# Create measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3)))

# Create structural model ----
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

# Estimate the model
corp_rep_pls_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm,
  structural_model  = corp_rep_sm,
  missing = mean_replacement,
  missing_value = "-99")

# Summarize the model results
summary_corp_rep <- summary(corp_rep_pls_model)

# Inspect iterations
summary_corp_rep$iterations

# Inspect the outer loadings
summary_corp_rep$loadings

# Inspect the indicator reliability
summary_corp_rep$loadings^2

# Inspect the internal consistency and reliability
summary_corp_rep$reliability

# Plot the reliabilities of constructs
plot(summary_corp_rep$reliability)

# Table of the FL criteria
summary_corp_rep$validity$fl_criteria

# HTMT Ratio
summary_corp_rep$validity$htmt

# Bootstrap the model
boot_corp_rep <- bootstrap_model(seminr_model = corp_rep_pls_model,
                                 nboot = 1000)

# Store the summary of the bootstrapped model
sum_boot_corp_rep <- summary(boot_corp_rep, alpha = 0.10)

# Extract the bootstrapped HTMT
sum_boot_corp_rep$bootstrapped_HTMT
