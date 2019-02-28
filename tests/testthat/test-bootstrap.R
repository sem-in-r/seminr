context("SEMinR correctly bootstraps the simple model\n")

# Test cases
## Simple case
set.seed(123)
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$bootstrapMatrix, file = "tests/fixtures/boostrapmatrix1.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boostrapmatrix1.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the interaction model\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights)
)

# interaction constructs must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$bootstrapMatrix, file = "tests/fixtures/boostrapmatrix2.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boostrapmatrix2.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model weights - composite measurement mode\n")

# Test cases
## Regular model

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$weights_descriptives

## Output originally created using following lines
# write.csv(bootmodel$boot_weights, file = "tests/fixtures/boot_weights.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_weights.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model loadings - factor measurement mode\n")

# Test cases
## Regular model

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_weighting)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$loadings_descriptives

## Output originally created using following lines
# write.csv(bootmodel$boot_loadings, file = "tests/fixtures/boot_loadings.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_loadings.csv", row.names = 1))

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
# write.csv(bootmodel$boot_HTMT, file = "tests/fixtures/boot_HTMT.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_HTMT.csv", row.names = 1))

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
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value")),
  paths(to = "Expectation", from = "Image")
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
paths <- bootmodel$boot_paths

## Output originally created using following lines
# write.csv(c(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction"),
#             confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1),
#             confidence_interval(bootmodel, from = "Image", to = "Satisfaction")),
#           file = "tests/fixtures/conf_ints.csv")

# Load controls
conf_ints_control <- as.matrix(read.csv("../fixtures/conf_ints.csv", row.names = 1))

# Testing
test_that("Seminr calculates the confidence intervals correctly", {
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[1]],
               conf_ints_control[1,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[2]],
               conf_ints_control[2,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[1]],
               conf_ints_control[3,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[2]],
               conf_ints_control[4,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", to = "Satisfaction")[[1]],
               conf_ints_control[5,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", to = "Satisfaction")[[2]],
               conf_ints_control[6,1], tolerance = 0.00001)
})
