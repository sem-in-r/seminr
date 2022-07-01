### Accompanying Code for:
## Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
## Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.

## Chapter 7: Mediation analysis

# Load the SEMinR library
library(seminr)

# Load the data
corp_rep_data <- corp_rep_data

# Create measurement model ----
corp_rep_mm_ext <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model ----
corp_rep_sm_ext <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)

# Estimate the model ----
corp_rep_pls_model_ext <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_ext,
  structural_model  = corp_rep_sm_ext,
  missing = mean_replacement,
  missing_value = "-99")

# Extract the summary
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)

# Bootstrap the model ----
boot_corp_rep_ext <- bootstrap_model(
  seminr_model = corp_rep_pls_model_ext,
  nboot = 10000,
  cores = parallel::detectCores(),
  seed = 123)

# Summarize the results of the bootstrap
summary_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.05)

# Inspect indirect effects
summary_corp_rep_ext$total_indirect_effects

# Obtain confidence intervals for indirect effects
total_indirect_ci(boot_corp_rep_ext, from = "COMP", to = "CUSL", alpha = 0.05)
total_indirect_ci(boot_corp_rep_ext, from = "LIKE", to = "CUSL", alpha = 0.05)
# Inspect the direct effects
summary_corp_rep_ext$paths

# Inspect the confidence intervals for direct effects
summary_boot_corp_rep_ext$bootstrapped_paths

# Calculate the sign of p1*p2*p3
summary_corp_rep_ext$paths["LIKE", "CUSL"] *
  summary_corp_rep_ext$paths["LIKE","CUSA"] *
  summary_corp_rep_ext$paths["CUSA","CUSL"]
