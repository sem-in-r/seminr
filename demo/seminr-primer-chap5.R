library(seminr)

# Run all demo code from Chapter 5
demo("seminr-primer-chap5")

# Loading the data
corp_rep_data <- corp_rep_data

# Cleaning the data ----
cleaned_data <- mean_replacement(data = corp_rep_data,
                                 missing_value_ind = -99)

# Creating measurement model ----
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

# Creating structural model ----
corp_rep_sm_ext <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)

# Estimating the model
corp_rep_pls_model_ext <- estimate_pls(data              = cleaned_data,
                                       measurement_model = corp_rep_mm_ext,
                                       structural_model  = corp_rep_sm_ext)

# Iterations to converge
corp_rep_pls_model_ext$iterations

# Redundancy analysis ----
# ATTR ----
# Creating measurement model
ATTR_redundancy_mm <- constructs(
  composite("ATTR_F", multi_items("attr_", 1:3), weights = mode_B),
  composite("ATTR_G", single_item("attr_global"))
)

# Creating structural model
ATTR_redundancy_sm <- relationships(
  paths(from = c("ATTR_F"), to = c("ATTR_G"))
)

# Estimating the model
ATTR_redundancy_pls_model <- estimate_pls(data              = cleaned_data,
                                          measurement_model = ATTR_redundancy_mm,
                                          structural_model  = ATTR_redundancy_sm)

# CSOR ----
# Creating measurement model
CSOR_redundancy_mm <- constructs(
  composite("CSOR_F", multi_items("csor_", 1:5), weights = mode_B),
  composite("CSOR_G", single_item("csor_global"))
)

# Creating structural model
CSOR_redundancy_sm <- relationships(
  paths(from = c("CSOR_F"), to = c("CSOR_G"))
)

# Estimating the model
CSOR_redundancy_pls_model <- estimate_pls(data              = cleaned_data,
                                          measurement_model = CSOR_redundancy_mm,
                                          structural_model  = CSOR_redundancy_sm)

# PERF ----
# Creating measurement model
PERF_redundancy_mm <- constructs(
  composite("PERF_F", multi_items("perf_", 1:5), weights = mode_B),
  composite("PERF_G", single_item("perf_global"))
)

# Creating structural model
PERF_redundancy_sm <- relationships(
  paths(from = c("PERF_F"), to = c("PERF_G"))
)

# Estimating the model
PERF_redundancy_pls_model <- estimate_pls(data              = cleaned_data,
                                          measurement_model = PERF_redundancy_mm,
                                          structural_model  = PERF_redundancy_sm)

# QUAL ----
# Creating measurement model
QUAL_redundancy_mm <- constructs(
  composite("QUAL_F", multi_items("qual_", 1:8), weights = mode_B),
  composite("QUAL_G", single_item("qual_global"))
)

# Creating structural model
QUAL_redundancy_sm <- relationships(
  paths(from = c("QUAL_F"), to = c("QUAL_G"))
)

# Estimating the model
QUAL_redundancy_pls_model <- estimate_pls(data              = cleaned_data,
                                          measurement_model = QUAL_redundancy_mm,
                                          structural_model  = QUAL_redundancy_sm)






















