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
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2)

# Load outputs
bootmatrix <- bootmodel$bootstrapMatrix

## Output originally created using following lines
# write.csv(bootmodel$bootstrapMatrix, file = "tests/fixtures/boostrapmatrix1.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boostrapmatrix1.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,1], bootmatrix_control[,1])
  diff <- abs(bootmatrix[,1] - bootmatrix_control[,1])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,2], bootmatrix_control[,2])
  diff <- abs(bootmatrix[,2] - bootmatrix_control[,2])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,3], bootmatrix_control[,3])
  diff <- abs(bootmatrix[,3] - bootmatrix_control[,3])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
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
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2)

# Load outputs
bootmatrix <- bootmodel$bootstrapMatrix

## Output originally created using following lines
# write.csv(bootmodel$bootstrapMatrix, file = "tests/fixtures/boostrapmatrix2.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boostrapmatrix2.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,1], bootmatrix_control[,1])
  diff <- abs(bootmatrix[,1] - bootmatrix_control[,1])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,2], bootmatrix_control[,2])
  diff <- abs(bootmatrix[,2] - bootmatrix_control[,2])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
#  expect_equal(bootmatrix[,3], bootmatrix_control[,3])
  diff <- abs(bootmatrix[,3] - bootmatrix_control[,3])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})
