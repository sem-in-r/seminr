# context("SEMinR correctly bootstraps simple models\n")

# Test cases
# ## One antecedent model cases
# set.seed(123)
#
# inn_mm <- constructs(
#   composite("INN_P", multi_items("IMAG", 1:4)),
#   composite("INN_M", multi_items("PERQ", 1:4)),
#   composite("THIRD", multi_items("CUEX", 1:3))
# )
#
# inn_sm <- relationships(paths(from = "INN_P", to = "INN_M"))
# pls <- estimate_pls(data = mobi, inn_mm, inn_sm)
# expect_error(bootstrap_model(seminr_model = pls, nboot = 5), NA)
#
# inn_sm <- relationships(paths(from = "INN_P", to = c("INN_M", "THIRD")))
# pls <- estimate_pls(data = mobi, inn_mm, inn_sm)
# expect_error(bootstrap_model(seminr_model = pls, nboot = 5), NA)


# Test cases
## Simple case
set.seed(123)

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(from = c("Image", "Expectation", "Value"),
        to = "Satisfaction")
)

mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_5_X/boostrapmatrix1.csv")     # V3.5.X
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_6_0/boostrapmatrix1.csv")     # V3.6.0

bootmatrix_control <- as.matrix(read.csv(file = paste(test_folder,"boostrapmatrix1.csv", sep = ""), row.names = 1))

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

path_diff <- test_path_diff(
  path(from="Image", to="Satisfaction"),
  path(from="Expectation", to="Satisfaction"),
  bootmodel
)

test_that("Seminr correctly tests the difference between paths", {
  expect_equal(path_diff$pval, 0.003111947, tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the interaction model\n")

# Test cases
## Interaction case

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal, weights = mode_A),
  interaction_term(iv = "Image", moderator = "Value", method = orthogonal, weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate using semPLS
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_5_X/boostrapmatrix2.csv")     # V3.5.X
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_6_0/boostrapmatrix2.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boostrapmatrix2.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model weights - composite measurement mode\n")

# Test cases
## Regular model

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$weights_descriptives

## Output originally created using following lines
# write.csv(bootmodel$weights_descriptives, file = "tests/fixtures/V_3_5_X/boot_weights.csv")     # V3.5.X
# write.csv(bootmodel$weights_descriptives, file = "tests/fixtures/V_3_6_0/boot_weights.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_weights.csv", sep = ""), row.names = 1))
# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model loadings - factor measurement mode\n")

# Test cases
## Regular model

mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_weighting)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$loadings_descriptives

## Output originally created using following lines
# write.csv(bootmodel$loadings_descriptives, file = "tests/fixtures/V_3_5_x/boot_loadings.csv")     # V3.5.X
# write.csv(bootmodel$loadings_descriptives, file = "tests/fixtures/V_3_6_0/boot_loadings.csv")     # V3.6.0


# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_loadings.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model HTMT - factor measurement mode\n")

# Test cases

# Load outputs
bootmatrix <- bootmodel$HTMT_descriptives

## Output originally created using following lines
# write.csv(bootmodel$HTMT_descriptives, file = "tests/fixtures/V_3_5_X/boot_HTMT.csv")         # V3.5.X
# write.csv(bootmodel$HTMT_descriptives, file = "tests/fixtures/V_3_6_0/boot_HTMT.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_HTMT.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly reports the confidence interval bootstrapped paths\n")

# Test cases
## Simple case
set.seed(123)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value")),
  paths(to = "Expectation", from = "Image")
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
paths <- bootmodel$boot_paths

## Output originally created using following lines
# write.csv(c(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction"),
#             confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1),
#             confidence_interval(bootmodel, from = "Image", to = "Satisfaction")),
#           file = "tests/fixtures/V_3_6_0/conf_ints.csv")

# Load controls
conf_ints_control <- as.matrix(read.csv(paste(test_folder,"conf_ints.csv", sep = ""), row.names = 1))


# Testing
test_that("Seminr calculates the confidence intervals correctly", {
  expect_equal(specific_effect_significance(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[5]],
               conf_ints_control[1,1], tolerance = 0.00001)
  expect_equal(specific_effect_significance(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[6]],
               conf_ints_control[2,1], tolerance = 0.00001)
  expect_equal(specific_effect_significance(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[5]],
               conf_ints_control[3,1], tolerance = 0.00001)
  expect_equal(specific_effect_significance(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[6]],
               conf_ints_control[4,1], tolerance = 0.00001)
  expect_equal(specific_effect_significance(bootmodel, from = "Image", to = "Satisfaction")[[5]],
               conf_ints_control[5,1], tolerance = 0.00001)
  expect_equal(specific_effect_significance(bootmodel, from = "Image", to = "Satisfaction")[[6]],
               conf_ints_control[6,1], tolerance = 0.00001)
})

# Test cases
context("SEMinR correctly reports the confidence interval bootstrapped paths\n")
# Bootstrap performs correctly with missing values, factorial method, and low stop criterion
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
  inner_weights = path_factorial,
  missing = mean_replacement,
  missing_value = "-99",
  stopCriterion = 1)

# Summarize model
summary_corp_rep_ext <- summary(corp_rep_pls_model_ext)

# Bootstrap model
boot_model <- bootstrap_model(corp_rep_pls_model_ext, nboot = 200, cores = 2, seed = 123)

# Summarize bootstrapped model
sum_boot_model <- summary(boot_model)

# Load outputs
paths <- sum_boot_model$bootstrapped_paths
descriptives <- summary_corp_rep_ext$descriptives$statistics$items

## Output originally created using following lines
# write.csv(sum_boot_model$bootstrapped_paths, file = "tests/fixtures/V_3_6_0/missing_cases_paths.csv")
# write.csv(summary_corp_rep_ext$descriptives$statistics$items, file = "tests/fixtures/V_3_6_0/missing_cases_item_descriptives.csv")

# Load controls
boot_paths_control <- as.matrix(read.csv(paste(test_folder,"missing_cases_paths.csv", sep = ""), row.names = 1))
boot_item_control <- as.matrix(read.csv(paste(test_folder,"missing_cases_item_descriptives.csv", sep = ""), row.names = 1))

# Testing
test_that("Seminr performs the bootstrapping correctly with missing values", {
  expect_equal(paths[, 1], boot_paths_control[, 1], tolerance = 0.00001)
  expect_equal(paths[, 2], boot_paths_control[, 2], tolerance = 0.00001)
  expect_equal(paths[, 3], boot_paths_control[, 3], tolerance = 0.00001)
  expect_equal(paths[, 4], boot_paths_control[, 4], tolerance = 0.00001)
  expect_equal(paths[, 5], boot_paths_control[, 5], tolerance = 0.00001)
  expect_equal(paths[, 6], boot_paths_control[, 6], tolerance = 0.00001)
})

test_that("Seminr calculates descriptives correctly with missing values", {
  expect_equal(descriptives[, 7], boot_item_control[, 7], tolerance = 0.00001)
  expect_equal(descriptives[, 2], boot_item_control[, 2], tolerance = 0.00001)
  expect_equal(descriptives[, 3], boot_item_control[, 3], tolerance = 0.00001)
  expect_equal(descriptives[, 4], boot_item_control[, 4], tolerance = 0.00001)
  expect_equal(descriptives[, 5], boot_item_control[, 5], tolerance = 0.00001)
  expect_equal(descriptives[, 6], boot_item_control[, 6], tolerance = 0.00001)
  expect_equal(descriptives[, 8], boot_item_control[, 8], tolerance = 0.00001)
  expect_equal(descriptives[, 9], boot_item_control[, 9], tolerance = 0.00001)
})
