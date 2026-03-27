context("SEMinR correctly generates PLS and LM predictions models\n")
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
# Create moderated measurement model ----
corp_rep_mm_mod <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  interaction_term("QUAL", "PERF", method = two_stage )
)

# Create moderated structural model ----
corp_rep_sm_mod <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "QUAL*PERF"), to = "COMP")
)

# LOOCV prediction tests — skipped on CRAN (344-fold cross-validation is too slow)
test_that("Seminr performs the DA prediction correctly for PLS and LM in and out sample", {
  skip_on_cran()

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

  # Load controls
  DA_control <- as.matrix(read.csv(file = paste(test_folder,"predict_pls_DA.csv", sep = ""), row.names = NULL))
  EA_control <- as.matrix(read.csv(file = paste(test_folder,"predict_pls_EA.csv", sep = ""), row.names = NULL))
  rownames(DA_control) <- rownames(EA_control) <- 1:8

  expect_equal(DA_control, DA_predictions, tolerance = 0.00001)
  expect_equal(EA_control, EA_predictions, tolerance = 0.00001)
})

context("predict.seminr_model correctly generates PLS predictions from two_stage moderated models\n")


# Estimate the model ----
corp_rep_pls_model_mod <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_mod,
  structural_model  = corp_rep_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

Results <- predict(object = corp_rep_pls_model_mod, testData = corp_rep_data2, technique = predict_EA)
# write.csv(Results$item_residuals, file = "tests/fixtures/V_3_6_0/two_stage_predict.csv")

# Load controls
two_stage_control <- as.matrix(read.csv(file = paste(test_folder,"two_stage_predict.csv", sep = ""), row.names = 1, check.names = FALSE))

test_that("Seminr estimates the construct scores correctly", {
  expect_equal(as.vector(unlist(Results$item_residuals)), as.vector(two_stage_control), tolerance = 0.00001)
})

context("predict.seminr_model generates predictions for product_indicator moderated models\n")

corp_rep_mm_prod <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  interaction_term("QUAL", "PERF", method = product_indicator)
)

# Estimate PI model ----
corp_rep_pls_model_prod <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_prod,
  structural_model  = corp_rep_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

test_that("predict() works for product_indicator models", {
  pred <- predict(object = corp_rep_pls_model_prod, testData = corp_rep_data2, technique = predict_EA)
  expect_s3_class(pred, "predicted_seminr_model")
  expect_equal(nrow(pred$predicted_items), nrow(corp_rep_data2))
  # Predictions should not be all NA or all zero
  expect_false(all(is.na(pred$predicted_items)))
  expect_false(all(pred$predicted_items == 0))
  # Residuals should exist and have correct dimensions
  expect_equal(nrow(pred$item_residuals), nrow(corp_rep_data2))
  expect_equal(ncol(pred$item_residuals), ncol(pred$predicted_items))
})

test_that("predict() with product_indicator works with both DA and EA techniques", {
  pred_DA <- predict(object = corp_rep_pls_model_prod, testData = corp_rep_data2, technique = predict_DA)
  pred_EA <- predict(object = corp_rep_pls_model_prod, testData = corp_rep_data2, technique = predict_EA)
  expect_s3_class(pred_DA, "predicted_seminr_model")
  expect_s3_class(pred_EA, "predicted_seminr_model")
})

context("predict.seminr_model generates predictions for orthogonal moderated models\n")

corp_rep_mm_orth <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  interaction_term("QUAL", "PERF", method = orthogonal)
)

# Estimate orthogonal model ----
corp_rep_pls_model_orth <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_orth,
  structural_model  = corp_rep_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

test_that("orthogonal model stores interaction_params with ortho_coefs", {
  expect_false(is.null(corp_rep_pls_model_orth$interaction_params))
  expect_false(is.null(corp_rep_pls_model_orth$interaction_params[["QUAL*PERF"]]))
  expect_false(is.null(corp_rep_pls_model_orth$interaction_params[["QUAL*PERF"]]$ortho_coefs))
  # Should have one coefficient vector per product indicator item
  n_qual_items <- length(construct_items(corp_rep_pls_model_orth$mmMatrix, "QUAL"))
  n_perf_items <- length(construct_items(corp_rep_pls_model_orth$mmMatrix, "PERF"))
  # Orthogonal interactions use same items as the base constructs (not the product items)
  # but the interaction construct has n_qual * n_perf product items
  expect_equal(length(corp_rep_pls_model_orth$interaction_params[["QUAL*PERF"]]$ortho_coefs),
               n_qual_items * n_perf_items)
})

test_that("predict() works for orthogonal models", {
  pred <- predict(object = corp_rep_pls_model_orth, testData = corp_rep_data2, technique = predict_EA)
  expect_s3_class(pred, "predicted_seminr_model")
  expect_equal(nrow(pred$predicted_items), nrow(corp_rep_data2))
  expect_false(all(is.na(pred$predicted_items)))
  expect_false(all(pred$predicted_items == 0))
  expect_equal(nrow(pred$item_residuals), nrow(corp_rep_data2))
})

test_that("product_indicator model stores interaction_params", {
  expect_false(is.null(corp_rep_pls_model_prod$interaction_params))
  expect_false(is.null(corp_rep_pls_model_prod$interaction_params[["QUAL*PERF"]]))
  # PI models should NOT have ortho_coefs
  expect_true(is.null(corp_rep_pls_model_prod$interaction_params[["QUAL*PERF"]]$ortho_coefs))
})

context("predict.seminr_model dispatch and edge cases\n")

test_that("detect_interaction_method identifies methods correctly", {
  expect_equal(unname(seminr:::detect_interaction_method(corp_rep_pls_model_mod)), "two_stage")
  expect_equal(unname(seminr:::detect_interaction_method(corp_rep_pls_model_prod)), "product_indicator")
  expect_equal(unname(seminr:::detect_interaction_method(corp_rep_pls_model_orth)), "orthogonal")
})

test_that("mixed interaction methods produce informative error", {
  mm_mixed <- constructs(
    composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
    composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
    composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
    composite("COMP", multi_items("comp_", 1:3)),
    interaction_term("QUAL", "PERF", method = two_stage),
    interaction_term("QUAL", "CSOR", method = product_indicator)
  )
  sm_mixed <- relationships(
    paths(from = c("QUAL", "PERF", "CSOR", "QUAL*PERF", "QUAL*CSOR"), to = "COMP")
  )
  suppressMessages(
    model_mixed <- estimate_pls(
      data = corp_rep_data,
      measurement_model = mm_mixed,
      structural_model  = sm_mixed,
      missing = mean_replacement,
      missing_value = "-99")
  )
  expect_error(predict(object = model_mixed, testData = corp_rep_data2, technique = predict_EA),
               "Mixed interaction methods")
})

test_that("PI and orthogonal predictions are reasonable relative to two_stage", {
  # All three methods should produce predictions in the same ballpark
  pred_ts <- predict(object = corp_rep_pls_model_mod, testData = corp_rep_data2, technique = predict_EA)
  pred_pi <- predict(object = corp_rep_pls_model_prod, testData = corp_rep_data2, technique = predict_EA)
  pred_orth <- predict(object = corp_rep_pls_model_orth, testData = corp_rep_data2, technique = predict_EA)

  # Compare RMSE of item residuals — should be in the same order of magnitude
  rmse_ts <- sqrt(mean(as.matrix(pred_ts$item_residuals)^2))
  rmse_pi <- sqrt(mean(as.matrix(pred_pi$item_residuals)^2))
  rmse_orth <- sqrt(mean(as.matrix(pred_orth$item_residuals)^2))

  # All RMSEs should be positive and finite
  expect_true(is.finite(rmse_ts) && rmse_ts > 0)
  expect_true(is.finite(rmse_pi) && rmse_pi > 0)
  expect_true(is.finite(rmse_orth) && rmse_orth > 0)
})

context("predict_pls cross-validation works for PI and orthogonal models\n")

test_that("predict_pls works with product_indicator model (k-fold)", {
  cv_pred <- predict_pls(model = corp_rep_pls_model_prod,
                         technique = predict_DA,
                         noFolds = 10)
  expect_s3_class(cv_pred, "predict_pls_model")
  expect_false(is.null(cv_pred$items$PLS_out_of_sample))
  expect_false(all(is.na(cv_pred$items$PLS_out_of_sample)))
})

test_that("predict_pls works with orthogonal model (k-fold)", {
  cv_pred <- predict_pls(model = corp_rep_pls_model_orth,
                         technique = predict_DA,
                         noFolds = 10)
  expect_s3_class(cv_pred, "predict_pls_model")
  expect_false(is.null(cv_pred$items$PLS_out_of_sample))
  expect_false(all(is.na(cv_pred$items$PLS_out_of_sample)))
})

context("Prediction edge cases: quadratic terms and multiple interactions\n")

test_that("predict() works with quadratic terms (product_indicator)", {
  mm_quad <- constructs(
    composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
    composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
    composite("COMP", multi_items("comp_", 1:3)),
    quadratic_term(iv = "QUAL", method = product_indicator)
  )
  sm_quad <- relationships(
    paths(from = c("QUAL", "PERF", "QUAL*QUAL"), to = "COMP")
  )
  suppressMessages(
    model_quad <- estimate_pls(
      data = corp_rep_data,
      measurement_model = mm_quad,
      structural_model  = sm_quad,
      missing = mean_replacement,
      missing_value = "-99")
  )
  pred <- predict(object = model_quad, testData = corp_rep_data2, technique = predict_DA)
  expect_s3_class(pred, "predicted_seminr_model")
  expect_false(all(is.na(pred$predicted_items)))
})

test_that("predict() works with multiple product_indicator interactions", {
  mm_multi <- constructs(
    composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
    composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
    composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
    composite("COMP", multi_items("comp_", 1:3)),
    interaction_term("QUAL", "PERF", method = product_indicator),
    interaction_term("QUAL", "CSOR", method = product_indicator)
  )
  sm_multi <- relationships(
    paths(from = c("QUAL", "PERF", "CSOR", "QUAL*PERF", "QUAL*CSOR"), to = "COMP")
  )
  suppressMessages(
    model_multi <- estimate_pls(
      data = corp_rep_data,
      measurement_model = mm_multi,
      structural_model  = sm_multi,
      missing = mean_replacement,
      missing_value = "-99")
  )
  pred <- predict(object = model_multi, testData = corp_rep_data2, technique = predict_DA)
  expect_s3_class(pred, "predicted_seminr_model")
  expect_equal(nrow(pred$predicted_items), nrow(corp_rep_data2))
})

context("predict_pls yields correct predictions for LM and PLS for moderated models.\n")

# Reuse two_stage model from above (corp_rep_pls_model_mod, same mm/sm)
nick <- predict(object = corp_rep_pls_model_mod,
                testData = corp_rep_data,
                technique = predict_DA)

pred_results <- predict_pls(model = corp_rep_pls_model_mod,
            technique = predict_DA,
            noFolds = NULL,
            reps = NULL
            )

sum_pred_results <- summary(pred_results)

# write.csv(unlist(sum_pred_results), file = "tests/fixtures/V_3_6_0/two_stage_predict_pls.csv")

# Load controls
two_stage_predict_pls_control <- as.matrix(read.csv(file = paste(test_folder,"two_stage_predict_pls.csv", sep = ""), row.names = 1, check.names = FALSE))
res <- unlist(sum_pred_results)
names(res) <- c()

test_that("Seminr generates the predicted scores correctly", {
  expect_equal(res[1:24],two_stage_predict_pls_control[1:24], tolerance = 0.000001)
})

# == Regression tests for non-standard rownames (GitHub issue #347) ==

context("predict_pls handles non-standard rownames (issue #347)\n")

# Shared model spec for rowname tests
rowname_mm <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

rowname_sm <- relationships(
  paths(from = c("QUAL", "PERF"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL"))
)

test_that("predict_pls with non-standard rownames matches standard rowname results", {
  data_standard <- corp_rep_data[1:100, ]
  rownames(data_standard) <- seq_len(nrow(data_standard))

  # Same data with non-sequential numeric rownames (as if subset from larger df)
  data_nonseq <- data_standard
  rownames(data_nonseq) <- seq(from = 10, by = 3, length.out = nrow(data_standard))

  # Same data with character rownames
  data_char <- data_standard
  rownames(data_char) <- paste0("obs_", seq_len(nrow(data_standard)))

  suppressMessages({
    model_std <- estimate_pls(
      data = data_standard,
      measurement_model = rowname_mm,
      structural_model = rowname_sm,
      missing = mean_replacement,
      missing_value = "-99"
    )
    model_nonseq <- estimate_pls(
      data = data_nonseq,
      measurement_model = rowname_mm,
      structural_model = rowname_sm,
      missing = mean_replacement,
      missing_value = "-99"
    )
    model_char <- estimate_pls(
      data = data_char,
      measurement_model = rowname_mm,
      structural_model = rowname_sm,
      missing = mean_replacement,
      missing_value = "-99"
    )
  })

  # Predict with identical shuffle (same seed) for each model
  set.seed(42)
  result_std <- predict_pls(model_std, technique = predict_DA, noFolds = 10)
  set.seed(42)
  result_nonseq <- predict_pls(model_nonseq, technique = predict_DA, noFolds = 10)
  set.seed(42)
  result_char <- predict_pls(model_char, technique = predict_DA, noFolds = 10)

  # Predictions should be numerically identical (ignoring rownames)
  expect_equal(`rownames<-`(result_nonseq$items$PLS_out_of_sample, NULL),
               `rownames<-`(result_std$items$PLS_out_of_sample, NULL))
  expect_equal(`rownames<-`(result_char$items$PLS_out_of_sample, NULL),
               `rownames<-`(result_std$items$PLS_out_of_sample, NULL))

  # Also verify reps path
  set.seed(42)
  reps_std <- predict_pls(model_std, technique = predict_DA, noFolds = 10, reps = 2)
  set.seed(42)
  reps_nonseq <- predict_pls(model_nonseq, technique = predict_DA, noFolds = 10, reps = 2)
  set.seed(42)
  reps_char <- predict_pls(model_char, technique = predict_DA, noFolds = 10, reps = 2)

  expect_equal(`rownames<-`(reps_nonseq$items$PLS_out_of_sample, NULL),
               `rownames<-`(reps_std$items$PLS_out_of_sample, NULL))
  expect_equal(`rownames<-`(reps_char$items$PLS_out_of_sample, NULL),
               `rownames<-`(reps_std$items$PLS_out_of_sample, NULL))
})
