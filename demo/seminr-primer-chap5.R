library(seminr)

# Loade the data
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
corp_rep_pls_model_ext <- estimate_pls(data              = corp_rep_data,
                                       measurement_model = corp_rep_mm_ext,
                                       structural_model  = corp_rep_sm_ext,
                                       missing = mean_replacement,
                                       missing_value = "-99")

# Iterations to converge
corp_rep_pls_model_ext$iterations

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

# Check the path coefficients for convergent validity
ATTR_redundancy_pls_model$path_coef
CSOR_redundancy_pls_model$path_coef
PERF_redundancy_pls_model$path_coef
QUAL_redundancy_pls_model$path_coef

# Collinearity analysis ----
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)
summary_corp_rep_ext$validity$vif_items

# Bootstrap the model ----
# seminr_model is the SEMinR model to be bootstrapped
# nboot is the number of bootstrap iterations to run
# cores is the number of cpu cores to use in multicore bootstrapping
# parallel::detectCores() allows for using the maximum cores on your device
# seed is the seed to be used for making sure the random process in
boot_corp_rep_ext <- bootstrap_model(seminr_model = corp_rep_pls_model_ext,
                                     nboot = 10000,
                                     cores = parallel::detectCores(),
                                     seed = 123)

# Summarize the results of the bootstrap
# alpha sets the specified level for significance, i.e. 0.05
summary_boot_corp_rep_ext <- summary(boot_corp_rep_ext,
                                     alpha = 0.05)

# Inspect the bootstrapping results for outer weights
summary_boot_corp_rep_ext$bootstrapped_weights
