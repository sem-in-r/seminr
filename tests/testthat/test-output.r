context("SEMinR correctly estimates the model orthogonally\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = "A"),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "A"),
  composite("Value",        multi_items("PERV", 1:2),weights = "A"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A")
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm,inner_weights = path.factorial)


# Load outputs
coefficients <- seminr_model$path_coef
factor_scores <- seminr_model$fscores
weight <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(seminr_model$path_coef, file = "tests/fixtures/coefficients.csv")
# write.csv(seminr_model$fscores, file = "tests/fixtures/factorscores.csv")
# write.csv(seminr_model$outer_weights, file = "tests/fixtures/weights.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/coefficients.csv", row.names = 1))
factor_scores_control <- as.matrix(read.csv("../fixtures/factorscores.csv")[,2:7])
weight_control <- as.matrix(read.csv("../fixtures/weights.csv", row.names=1))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,6], coefficients_control[,6])
})

test_that("Seminr estimates the factor scores correctly", {
  expect_equal(factor_scores[,1:6], factor_scores_control[,1:6])
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control)
})

context("SEMinR correctly estimates the model scaled product indicator\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = "A"),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = "A"),
  composite("Value",        multi_items("PERV", 1:2),weights = "A"),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = "A")
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_scaled("Image", "Expectation"),
  interaction_scaled("Image", "Value")
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm,inner_weights = path.factorial)


# Load outputs
coefficients <- seminr_model$path_coef
factor_scores <- seminr_model$fscores
weight <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(seminr_model$path_coef, file = "tests/fixtures/coefficients2.csv")
# write.csv(seminr_model$fscores, file = "tests/fixtures/factorscores2.csv")
# write.csv(seminr_model$outer_weights, file = "tests/fixtures/weights2.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/coefficients2.csv", row.names = 1))
factor_scores_control <- as.matrix(read.csv("../fixtures/factorscores2.csv")[,2:7])
weight_control <- as.matrix(read.csv("../fixtures/weights2.csv", row.names=1))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,6], coefficients_control[,6])
})

test_that("Seminr estimates the factor scores correctly", {
  expect_equal(factor_scores[,1:6], factor_scores_control[,1:6])
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control)
})

