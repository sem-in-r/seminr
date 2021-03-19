### Accompanying Code for:
## Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
## Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.

## Chapter 5: Evaluation of formative measurement models

# Load the SEMinR library
library(seminr)

# Load the corporate repuation data
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

# Summarize the model results
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)

# Iterations to converge
summary_corp_rep_ext$iterations

# Bootstrap the model
boot_corp_rep_ext <- bootstrap_model(seminr_model = corp_rep_pls_model_ext,
                                     nboot = 1000)

# Store the summary of the bootstrapped model
sum_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.10)

# Inspect the indicator loadings
summary_corp_rep_ext$loadings

# Inspect the indicator reliability
summary_corp_rep_ext$loadings^2

# Inspect the internal consistency and reliability
summary_corp_rep_ext$reliability

# Table of the FL criteria
summary_corp_rep_ext$validity$fl_criteria

# HTMT Ratio
summary_corp_rep_ext$validity$htmt

# Extract the bootstrapped HTMT
sum_boot_corp_rep_ext$bootstrapped_HTMT

# Redundancy analysis ----
# ATTR ----
# Create measurement model
ATTR_redundancy_mm <- constructs(
  composite("ATTR_F", multi_items("attr_", 1:3), weights = mode_B),
  composite("ATTR_G", single_item("attr_global"))
)

# Create structural model
ATTR_redundancy_sm <- relationships(
  paths(from = c("ATTR_F"), to = c("ATTR_G"))
)

# Estimate the model
ATTR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = ATTR_redundancy_mm,
                                          structural_model  = ATTR_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")

# Summarize the model
sum_ATTR_red_model <- summary(ATTR_redundancy_pls_model)

# CSOR ----
# Create measurement model
CSOR_redundancy_mm <- constructs(
  composite("CSOR_F", multi_items("csor_", 1:5), weights = mode_B),
  composite("CSOR_G", single_item("csor_global"))
)

# Create structural model
CSOR_redundancy_sm <- relationships(
  paths(from = c("CSOR_F"), to = c("CSOR_G"))
)

# Estimate the model
CSOR_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = CSOR_redundancy_mm,
                                          structural_model  = CSOR_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")

# Summarize the model
sum_CSOR_red_model <- summary(CSOR_redundancy_pls_model)

# PERF ----
# Create measurement model
PERF_redundancy_mm <- constructs(
  composite("PERF_F", multi_items("perf_", 1:5), weights = mode_B),
  composite("PERF_G", single_item("perf_global"))
)

# Create structural model
PERF_redundancy_sm <- relationships(
  paths(from = c("PERF_F"), to = c("PERF_G"))
)

# Estimate the model
PERF_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = PERF_redundancy_mm,
                                          structural_model  = PERF_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")

# Summarize the model
sum_PERF_red_model <- summary(PERF_redundancy_pls_model)

# QUAL ----
# Create measurement model
QUAL_redundancy_mm <- constructs(
  composite("QUAL_F", multi_items("qual_", 1:8), weights = mode_B),
  composite("QUAL_G", single_item("qual_global"))
)

# Create structural model
QUAL_redundancy_sm <- relationships(
  paths(from = c("QUAL_F"), to = c("QUAL_G"))
)

# Estimate the model
QUAL_redundancy_pls_model <- estimate_pls(data = corp_rep_data,
                                          measurement_model = QUAL_redundancy_mm,
                                          structural_model  = QUAL_redundancy_sm,
                                          missing = mean_replacement,
                                          missing_value = "-99")

# Summarize the model
sum_QUAL_red_model <- summary(QUAL_redundancy_pls_model)

# Check the path coefficients for convergent validity
sum_ATTR_red_model$paths
sum_CSOR_red_model$paths
sum_PERF_red_model$paths
sum_QUAL_red_model$paths

# Collinearity analysis ----
summary_corp_rep_ext$validity$vif_items

# Bootstrap the model ----
# seminr_model is the SEMinR model to be bootstrapped
# nboot is the number of bootstrap iterations to run
# cores is the number of cpu cores to use in multicore bootstrapping
# parallel::detectCores() allows for using the maximum cores on your device
# seed is the seed to be used for making bootstrap replicable
boot_corp_rep_ext <- bootstrap_model(
  seminr_model = corp_rep_pls_model_ext,
  nboot = 1000,
  cores = parallel::detectCores(),
  seed = 123)

# Summarize the results of the bootstrap
# alpha sets the specified level for significance, i.e. 0.05
sum_boot_corp_rep_ext <- summary(boot_corp_rep_ext, alpha = 0.05)

# Inspect the bootstrapping results for outer weights
sum_boot_corp_rep_ext$bootstrapped_weights

# Inspect the bootstrapping results for the outer loadings
sum_boot_corp_rep_ext$bootstrapped_loadings
