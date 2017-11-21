context("SEMinR correctly estimates rhoA for the simple model\n")

# Test cases
## Simple case
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner.weights = path.factorial)


# Load outputs
rho <- rhoA(seminr_model)

## Output originally created using following lines
# write.csv(rho, file = "tests/fixtures/rho1.csv")

# Load controls
rho_control <- as.matrix(read.csv("../fixtures/rho1.csv", row.names = 1))

# Testing

test_that("Seminr estimates rhoA correctly\n", {
  expect_equal(rho, rho_control)
})

context("SEMinR correctly estimates rhoA for the interaction model\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_ortho("Image", "Expectation"),
  interaction_ortho("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm,inner.weights = path.factorial)

# Load outputs
rho <- rhoA(seminr_model)

## Output originally created using following lines
# write.csv(rho, file = "tests/fixtures/rho2.csv")

# Load controls
rho_control <- as.matrix(read.csv("../fixtures/rho2.csv", row.names = 1))

# Testing

test_that("Seminr estimates rhoA correctly\n", {
  expect_equal(rho, rho_control)
})

context("SEMinR correctly estimates PLSc path coefficients and loadings for the simple model\n")

# Test cases
## Simple case
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner.weights = path.factorial)
# plscModel <- PLSc(seminr_model)

# Load outputs
path_coef <- seminr_model$path_coef
loadings <- seminr_model$outer_loadings

## Output originally created using following lines
# write.csv(path_coef, file = "tests/fixtures/path_coef1.csv")
# write.csv(loadings, file = "tests/fixtures/loadings1.csv")

# Load controls
path_coef_control <- as.matrix(read.csv("../fixtures/path_coef1.csv", row.names = 1))
loadings_control <- as.matrix(read.csv("../fixtures/loadings1.csv", row.names = 1))

# Testing

test_that("Seminr estimates PLSc path coefficients correctly\n", {
  expect_equal(path_coef, path_coef_control)
})

test_that("Seminr estimates PLSc loadings correctly\n", {
  expect_equal(loadings, loadings_control)
})

# redundant as you cannot perform PLSc on a moderated model
#context("SEMinR correctly estimates PLSc path coefficients and loadings for the interaction model\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
#mobi_mm <- constructs(
#  reflective("Image",        multi_items("IMAG", 1:5)),
#  reflective("Expectation",  multi_items("CUEX", 1:3)),
#  reflective("Value",        multi_items("PERV", 1:2)),
#  reflective("Satisfaction", multi_items("CUSA", 1:3))
#)

# interaction factors must be created after the measurement model is defined
#mobi_xm <- interactions(
#  interaction_ortho("Image", "Expectation"),
#  interaction_ortho("Image", "Value")
#)
#
# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
#mobi_sm <- relationships(
#  paths(to = "Satisfaction",
#        from = c("Image", "Expectation", "Value",
#                 "Image.Expectation", "Image.Value"))
#)

# Load data, assemble model, and estimate using semPLS
#data("mobi", package = "semPLS")
#seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm)
# plscModel <- PLSc(seminr_model)

# Load outputs
#path_coef <- seminr_model$path_coef
#loadings <- seminr_model$outer_loadings

## Output originally created using following lines
# write.csv(path_coef, file = "tests/fixtures/path_coef2.csv")
# write.csv(loadings, file = "tests/fixtures/loadings2.csv")

# Load controls
#path_coef_control <- as.matrix(read.csv("../fixtures/path_coef2.csv", row.names = 1))
#loadings_control <- as.matrix(read.csv("../fixtures/loadings2.csv", row.names = 1))

# Testing

#test_that("Seminr estimates PLSc path coefficients correctly\n", {
#  expect_equal(path_coef, path_coef_control)
#})

#test_that("Seminr estimates PLSc loadings correctly\n", {
#  expect_equal(loadings, loadings_control)
#})
