# context("SEMinR correctly generates PLS and LM predictions models\n")
set.seed(123)

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

# Generate the model predictions
predict_corp_rep_ext <- predict_pls(
  model = corp_rep_pls_model_ext,
  technique = predict_DA,
  noFolds = 344,
  reps = NULL)

predict_corp_rep_ext_EA <- predict_pls(
  model = corp_rep_pls_model_ext,
  technique = predict_EA,
  noFolds = 344,
  reps = NULL)

# Summarize the prediction results
sum_predict_corp_rep_ext <- summary(predict_corp_rep_ext)
sum_predict_corp_rep_ext_EA <- summary(predict_corp_rep_ext_EA)

DA_predictions <- rbind(sum_predict_corp_rep_ext$PLS_in_sample,
                        sum_predict_corp_rep_ext$PLS_out_of_sample,
                        sum_predict_corp_rep_ext$LM_in_sample,
                        sum_predict_corp_rep_ext$LM_out_of_sample)
EA_predictions <- rbind(sum_predict_corp_rep_ext_EA$PLS_in_sample,
                        sum_predict_corp_rep_ext_EA$PLS_out_of_sample,
                        sum_predict_corp_rep_ext_EA$LM_in_sample,
                        sum_predict_corp_rep_ext_EA$LM_out_of_sample)
rownames(DA_predictions) <- rownames(EA_predictions) <- 1:8
# Fixtures were generated with this code
# write.csv(rbind(sum_predict_corp_rep_ext$PLS_in_sample,
#                 sum_predict_corp_rep_ext$PLS_out_of_sample,
#                 sum_predict_corp_rep_ext$LM_in_sample,
#                 sum_predict_corp_rep_ext$LM_out_of_sample), file = "tests/fixtures/predict_pls_DA.csv")
# write.csv(rbind(sum_predict_corp_rep_ext_EA$PLS_in_sample,
#                 sum_predict_corp_rep_ext_EA$PLS_out_of_sample,
#                 sum_predict_corp_rep_ext_EA$LM_in_sample,
#                 sum_predict_corp_rep_ext_EA$LM_out_of_sample), file = "tests/fixtures/predict_pls_EA.csv")

# Load controls
DA_control <- as.matrix(read.csv(file = paste(test_folder,"predict_pls_DA.csv", sep = ""), row.names = NULL))
EA_control <- as.matrix(read.csv(file = paste(test_folder,"predict_pls_EA.csv", sep = ""), row.names = NULL))
rownames(DA_control) <- rownames(EA_control) <- 1:8

# Testing
test_that("Seminr performs the DA prediction correctly for PLS and LM in and out sample", {
  expect_equal(DA_control, DA_predictions, tolerance = 0.00001)
  expect_equal(EA_control, EA_predictions, tolerance = 0.00001)
})
