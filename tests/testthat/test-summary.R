context("SEMinR correctly returns the summary object for class seminr_model\n")

set.seed(1)
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
summary_object <- summary(seminr_model)

# Load outputs
# Remove HTMT
#htmt <- summary_object$metrics$Validity$HTMT
cross_loadings <- summary_object$cross_loadings
reliability <- summary_object$reliability

## Output originally created using following lines
# write.csv(summary_object$metrics$Validity$HTMT, file = "tests/fixtures/htmt.csv")
# write.csv(summary_object$cross_loadings, file = "tests/fixtures/cross_loadings.csv")
# write.csv(summary_object$reliability, file = "tests/fixtures/reliability.csv")

# Remove HTMT
#htmt_control <- as.matrix(read.csv("../fixtures/htmt.csv", row.names = 1))
cross_loadings_control <- as.matrix(read.csv("../fixtures/cross_loadings.csv", row.names = 1))
reliability_control <- as.matrix(read.csv("../fixtures/reliability.csv", row.names=1))

# Testing

# Remove HTMT
#test_that("Seminr estimates the htmt correctly", {
#  expect_equal(htmt, htmt_control)
#})

test_that("Seminr estimates the cross-loadings correctly", {
  expect_equal(cross_loadings, cross_loadings_control, tolerance = 0.00001)
})

test_that("Seminr estimates the reliability correctly", {
  expect_equal(reliability, reliability_control, tolerance = 0.00001)
})

context("SEMinR correctly returns the summary object for class boot_seminr_model\n")

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using estimate_pls
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
boot_seminr_model <- bootstrap_model(seminr_model, nboot = 500,cores = 2, seed = 123)
summary_object <- summary(boot_seminr_model)

# Load outputs
t_values <- summary_object$t_values
p_values <- summary_object$p_values

## Output originally created using following lines
# write.csv(summary_object$t_values, file = "tests/fixtures/tvalues.csv")
# write.csv(summary_object$p_values, file = "tests/fixtures/pvalues.csv")


t_values_control <- as.matrix(read.csv("../fixtures/tvalues.csv", row.names = 1))
p_values_control <- as.matrix(read.csv("../fixtures/pvalues.csv", row.names = 1))


# Testing

test_that("Seminr estimates the t-values correctly", {
  expect_equal(t_values, t_values_control, tolerance = 0.00001)
})

test_that("Seminr estimates the p-values correctly", {
  expect_equal(p_values, p_values_control, tolerance = 0.00001)
})

context("SEMinR:::evaluate_measurement_model() correctly evaluates FACTORS for class seminr_model\n")

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
boot_seminr_model <- bootstrap_model(seminr_model, nboot = 500,cores = 2, seed = 123)
utils::capture.output(summary_object <- seminr:::evaluate_measurement_model(seminr_model))
utils::capture.output(boot_summary_object <- seminr:::boot_evaluate_measurement_model(boot_seminr_model))

# Load outputs
factor_reliability <- summary_object$factor_reliability
factor_indicator_reliability <- summary_object$factor_indicator_reliability
factor_discriminant_validity <- summary_object$factor_discriminant_validity

## Output originally created using following lines
#write.csv(summary_object$factor_reliability, file = "tests/fixtures/factor_reliability.csv")
#write.csv(summary_object$factor_indicator_reliability, file = "tests/fixtures/factor_indicator_reliability.csv")
#write.csv(summary_object$factor_discriminant_validity, file = "tests/fixtures/factor_discriminant_validity.csv")

factor_reliability_control <- as.matrix(read.csv("../fixtures/factor_reliability.csv", row.names = 1))
factor_indicator_reliability_control <- as.matrix(read.csv("../fixtures/factor_indicator_reliability.csv", row.names = 1))
factor_discriminant_validity_control <- as.matrix(read.csv("../fixtures/factor_discriminant_validity.csv", row.names = 1))

# Testing

test_that("Seminr evaluates the factor reliability correctly", {
  expect_equal(factor_reliability,factor_reliability_control, tolerance = 0.00001)
})

test_that("Seminr evaluates the factor indicator reliability correctly", {
  expect_equal(factor_indicator_reliability,factor_indicator_reliability_control, tolerance = 0.00001)
})

test_that("Seminr evaluates the factor reliability correctly", {
  expect_equal(factor_discriminant_validity,factor_discriminant_validity_control, tolerance = 0.00001)
})

context("SEMinR:::evaluate_measurement_model() correctly evaluates COMPOSITES for class seminr_model\n")

# Load outputs
composite_indicator_reliability <- summary_object$composite_indicator_reliability
composite_collinearity <- unlist(summary_object$composite_collinearity)

## Output originally created using following lines
#write.csv(summary_object$composite_indicator_reliability, file = "tests/fixtures/composite_indicator_reliability.csv")
#write.csv(unlist(summary_object$composite_collinearity), file = "tests/fixtures/composite_collinearity.csv")

composite_indicator_reliability_control <- as.matrix(read.csv("../fixtures/composite_indicator_reliability.csv", row.names = 1))
composite_collinearity_control <- read.csv("../fixtures/composite_collinearity.csv", row.names = 1)

# Testing

test_that("Seminr evaluates the composite indicator reliability correctly", {
  expect_equal(composite_indicator_reliability,composite_indicator_reliability_control, tolerance = 0.00001)
})

test_that("Seminr evaluates the composite collinearity correctly", {
  expect_equal(unname(composite_collinearity[1:5]),composite_collinearity_control[1:5,1], tolerance = 0.00001)
})

context("SEMinR:::boot_evaluate_measurement_model() correctly evaluates FACTORS for class boot_seminr_model\n")

# Load outputs
factor_discriminant_validity_t_values <- boot_summary_object$factor_discriminant_validity_t_values
factor_discriminant_validity_p_values <- boot_summary_object$factor_discriminant_validity_p_values

## Output originally created using following lines
#write.csv(boot_summary_object$factor_discriminant_validity_t_values, file = "tests/fixtures/factor_discriminant_validity_t_values.csv")
#write.csv(boot_summary_object$factor_discriminant_validity_p_values, file = "tests/fixtures/factor_discriminant_validity_p_values.csv")

factor_discriminant_validity_t_values_control <- as.matrix(read.csv("../fixtures/factor_discriminant_validity_t_values.csv", row.names = 1))
factor_discriminant_validity_p_values_control <- as.matrix(read.csv("../fixtures/factor_discriminant_validity_p_values.csv", row.names = 1))

# Testing

test_that("Seminr evaluates the factor discriminant validity t_values control correctly", {
  expect_equal(factor_discriminant_validity_t_values, factor_discriminant_validity_t_values_control, tolerance = 0.00001)
})

test_that("Seminr evaluates the factor discriminant validity p_values correctly", {
  expect_equal(factor_discriminant_validity_p_values, factor_discriminant_validity_p_values_control, tolerance = 0.00001)
})

context("SEMinR:::boot_evaluate_measurement_model() correctly evaluates COMPOSITES for class boot_seminr_model\n")

# Load outputs
composite_indicator_weights_t_values <- boot_summary_object$composite_indicator_weights_t_values
composite_indicator_weights_p_values <- boot_summary_object$composite_indicator_weights_p_values

## Output originally created using following lines
#write.csv(boot_summary_object$composite_indicator_weights_t_values, file = "tests/fixtures/composite_indicator_weights_t_values.csv")
#write.csv(boot_summary_object$composite_indicator_weights_p_values, file = "tests/fixtures/composite_indicator_weights_p_values.csv")

composite_indicator_weights_t_values_control <- as.matrix(read.csv("../fixtures/composite_indicator_weights_t_values.csv", row.names = 1))
composite_indicator_weights_p_values_control <- as.matrix(read.csv("../fixtures/composite_indicator_weights_p_values.csv", row.names = 1))

# Testing

test_that("Seminr evaluates the composite indicator t values correctly", {
  expect_equal(composite_indicator_weights_t_values, composite_indicator_weights_t_values_control, tolerance = 0.00001)
})

test_that("Seminr evaluates the composite collinearity correctly", {
  expect_equal(composite_indicator_weights_p_values, composite_indicator_weights_p_values_control, tolerance = 0.00001)
})
