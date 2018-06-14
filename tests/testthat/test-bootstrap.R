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
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2)

# Load outputs
bootmatrix <- bootmodel$boot_weights

## Output originally created using following lines
# write.csv(bootmodel$boot_weights, file = "tests/fixtures/boot_weights.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_weights.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,1], bootmatrix_control[,1])
  diff <- abs(bootmatrix[1:5,1] - bootmatrix_control[1:5,1])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,2], bootmatrix_control[,2])
  diff <- abs(bootmatrix[6:8,6] - bootmatrix_control[6:8,6])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,3], bootmatrix_control[,3])
  diff <- abs(bootmatrix[9:10,11] - bootmatrix_control[9:10,11])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
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
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2)

# Load outputs
bootmatrix <- bootmodel$boot_loadings

## Output originally created using following lines
# write.csv(bootmodel$boot_loadings, file = "tests/fixtures/boot_loadings.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_loadings.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,1], bootmatrix_control[,1])
  diff <- abs(bootmatrix[1:5,1] - bootmatrix_control[1:5,1])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,2], bootmatrix_control[,2])
  diff <- abs(bootmatrix[6:8,6] - bootmatrix_control[6:8,6])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
  expect_lt(diff[[3]], 0.03)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,3], bootmatrix_control[,3])
  diff <- abs(bootmatrix[9:10,11] - bootmatrix_control[9:10,11])
  expect_lt(diff[[1]], 0.03)
  expect_lt(diff[[2]], 0.03)
})

context("SEMinR correctly bootstraps the model HTMT - factor measurement mode\n")

# Test cases
## Regular model
## previous model

# Load outputs
bootmatrix <- bootmodel$boot_HTMT

## Output originally created using following lines
# write.csv(bootmodel$boot_HTMT, file = "tests/fixtures/boot_HTMT.csv")

# Load controls
bootmatrix_control <- as.matrix(read.csv("../fixtures/boot_HTMT.csv", row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,1], bootmatrix_control[,1])
  diff <- abs(bootmatrix[1:4,4] - bootmatrix_control[1:4,4])
  expect_lt(diff[[1]], 0.04)
  expect_lt(diff[[2]], 0.04)
  expect_lt(diff[[3]], 0.04)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,2], bootmatrix_control[,2])
  diff <- abs(bootmatrix[1:2,7] - bootmatrix_control[1:2,7])
  expect_lt(diff[[1]], 0.04)
  expect_lt(diff[[2]], 0.04)
})

test_that("Seminr performs the bootstrapping correctly", {
  #  expect_equal(bootmatrix[,3], bootmatrix_control[,3])
  diff <- abs(bootmatrix[1:3,12] - bootmatrix_control[1:3,12])
  expect_lt(diff[[1]], 0.04)
  expect_lt(diff[[2]], 0.04)
  expect_lt(diff[[3]], 0.04)
})



