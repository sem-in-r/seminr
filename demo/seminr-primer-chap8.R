### Accompanying Code for:
## Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
## Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.

## Chapter 8: Moderation analysis

# Load the SEMinR library
library(seminr)

# Load the data
corp_rep_data <- corp_rep_data

# Create measurement model ----
corp_rep_mm_mod <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("SC", multi_items("switch_", 1:4)),
  composite("CUSL", multi_items("cusl_", 1:3)),
  interaction_term(iv = "CUSA", moderator = "SC", method = two_stage, weights = mode_A))

# Create structural model ----
corp_rep_sm_mod <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA", "SC", "CUSA*SC"), to = c("CUSL"))
)

# Estimate the model ----
corp_rep_pls_model_mod <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_mod,
  structural_model  = corp_rep_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

# Extract the summary
sum_corp_rep_mod <- summary(corp_rep_pls_model_mod)

# Bootstrap the model ----
boot_corp_rep_mod <- bootstrap_model(seminr_model = corp_rep_pls_model_mod,
                                     nboot = 1000)

# Summarize the results of the bootstrap
sum_boot_corp_rep_mod <- summary(boot_corp_rep_mod, alpha = 0.05)

# Inspect the bootstrapped structural paths
sum_boot_corp_rep_mod$bootstrapped_paths

# Simple slope analysis plot
slope_analysis(
  moderated_model = corp_rep_pls_model_mod,
  dv = "CUSL",
  moderator = "SC",
  iv = "CUSA",
  leg_place = "bottomright")
