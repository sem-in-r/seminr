context("SEMinR correctly returns the summary object for class seminr_model\n")

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = "A"),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "A"),
  composite("Value",        multi_items("PERV", 1:2),weights = "A"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A")
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
reliability <- summary_object$metrics$Reliability

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
  expect_equal(cross_loadings, cross_loadings_control)
})

test_that("Seminr estimates the reliability correctly", {
  expect_equal(reliability, reliability_control)
})

context("SEMinR correctly returns the summary object for class boot_seminr_model\n")

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = "A"),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "A"),
  composite("Value",        multi_items("PERV", 1:2),weights = "A"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A")
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
boot_seminr_model <- bootstrap_model(seminr_model, nboot = 500,cores = 2)
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
  diff <- abs(t_values[,1] / t_values_control[,1])
  expect_gt(diff[[1]], 0.9)
  expect_lt(diff[[1]], 1.1)
  expect_gt(diff[[2]], 0.9)
  expect_lt(diff[[2]], 1.1)
  expect_gt(diff[[3]], 0.9)
  expect_lt(diff[[3]], 1.1)

})

test_that("Seminr estimates the p-values correctly", {
  diff <- abs(p_values[,1] - p_values_control[,1])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})
