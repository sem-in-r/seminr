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
corp_rep_pls_model_ext <- estimate_pls(data              = corp_rep_data,
                                       measurement_model = corp_rep_mm_ext,
                                       structural_model  = corp_rep_sm_ext,
                                       missing = mean_replacement,
                                       missing_value = "-99")

# Extract the summary
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)

# Bootstrap the model ----
boot_corp_rep_ext <- bootstrap_model(seminr_model = corp_rep_pls_model_ext,
                                     nboot = 10000,
                                     cores = parallel::detectCores(),
                                     seed = 123)

# Summarize the results of the bootstrap
summary_boot_corp_rep_ext <- summary(boot_corp_rep_ext,
                                     alpha = 0.05)

# Inspect the structural model collinearity VIF
summary_corp_rep_ext$vif_antecedents

# Inspect the structural paths
summary_boot_corp_rep_ext$bootstrapped_paths

# Inspect the total effects
summary_boot_corp_rep_ext$bootstrapped_total_paths

# Inspect the model RSquares
summary_corp_rep_ext$paths

# Inspect the effect sizes
summary_corp_rep_ext$fSquare

# Generate the model predictions
predict_corp_rep_ext <- predict_pls(
  model = corp_rep_pls_model_ext,
  technique = predict_DA,
  noFolds = 10,
  reps = 10)

# Summarize the prediction results
sum_predict_corp_rep_ext <- summary(predict_corp_rep_ext)

# Analyze the distribution of prediction error
par(mfrow=c(1,3))
plot(sum_predict_corp_rep_ext, indicator = "cusl_1")
plot(sum_predict_corp_rep_ext, indicator = "cusl_2")
plot(sum_predict_corp_rep_ext, indicator = "cusl_3")

# Inspect the results of PLSpredict
sum_predict_corp_rep_ext

# Estimate alternative models
# Create measurement model ----
measurement_model <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural models
# Model 1
structural_model1 <- relationships(
  paths(from = c("QUAL","PERF","CSOR","ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP","LIKE"),  to = c("CUSA", "CUSL")),
  paths(from = "CUSA", to = c("CUSL"))
)
# Model 2
structural_model2 <- relationships(
  paths(from = c("QUAL","PERF","CSOR","ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP","LIKE"),  to = c("CUSA")),
  paths(from = c("LIKE"),  to = c("CUSL")),
  paths(from = "CUSA", to = c("CUSL"))
)
# Model 3
structural_model3 <- relationships(
  paths(from = c("QUAL","PERF","CSOR","ATTR"),        to = c("COMP", "LIKE")),
  paths(from = c("COMP","LIKE"),  to = c("CUSA")),
  paths(from = "CUSA",      to = c("CUSL"))
)

# Estimate and summarize the models ----
pls_model1 <- estimate_pls(data  = corp_rep_data,
                           measurement_model = measurement_model,
                           structural_model  = structural_model1)
sum_model1 <- summary(pls_model1)
pls_model2 <- estimate_pls(data  = corp_rep_data,
                           measurement_model = measurement_model,
                           structural_model  = structural_model2)
sum_model2 <- summary(pls_model2)
pls_model3 <- estimate_pls(data  = corp_rep_data,
                           measurement_model = measurement_model,
                           structural_model  = structural_model3)
sum_model3 <- summary(pls_model3)

# Inspect the IT Criteria matrix of Model1
sum_model1$it_criteria

# Subset the matrix to only return the BIC row and CUSL column
sum_model1$it_criteria["BIC", "CUSL"]

# Collect the vector of BIC values for CUSL
itcriteria_vector <- c(sum_model1$it_criteria["BIC","CUSL"],
                       sum_model2$it_criteria["BIC","CUSL"],
                       sum_model3$it_criteria["BIC","CUSL"])

# Assign the model names to IT Criteria vector
names(itcriteria_vector) <- c("Model1", "Model2", "Model3")

# Inspect the IT Criteria vector for competing models.
itcriteria_vector

# Calculate the model BIC Akaike Weights
compute_itcriteria_weights(itcriteria_vector)
