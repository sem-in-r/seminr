context("SEMinR correctly estimates the model orthogonally\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm,inner_weights = path_factorial)


# Load outputs
coefficients <- seminr_model$path_coef
construct_scores <- seminr_model$construct_scores
weight <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(seminr_model$path_coef, file = "tests/fixtures/coefficients.csv")
# write.csv(seminr_model$construct_scores, file = "tests/fixtures/constructscores.csv")
# write.csv(seminr_model$outer_weights, file = "tests/fixtures/weights.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/coefficients.csv", row.names = 1, check.names = FALSE))
construct_scores_control <- as.matrix(read.csv("../fixtures/constructscores.csv", row.names = 1, check.names = FALSE))
weight_control <- as.matrix(read.csv("../fixtures/weights.csv", row.names=1, check.names = FALSE))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,6], coefficients_control[,6], tolerance = 0.00001)
})

test_that("Seminr estimates the construct scores correctly", {
  expect_equal(construct_scores[,1:6], construct_scores_control[,1:6], tolerance = 0.00001)
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control, tolerance = 0.00001)
})

context("SEMinR correctly estimates the model scaled product indicator\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

# interaction constructs must be created after the measurement model is defined
mobi_xm <- interactions(
  interaction_scaled("Image", "Expectation"),
  interaction_scaled("Image", "Value")
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_xm, mobi_sm,inner_weights = path_factorial)


# Load outputs
coefficients <- seminr_model$path_coef
construct_scores <- seminr_model$construct_scores
weight <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(seminr_model$path_coef, file = "tests/fixtures/coefficients2.csv")
# write.csv(seminr_model$construct_scores, file = "tests/fixtures/constructscores2.csv")
# write.csv(seminr_model$outer_weights, file = "tests/fixtures/weights2.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/coefficients2.csv", row.names = 1, check.names = FALSE))
construct_scores_control <- as.matrix(read.csv("../fixtures/constructscores2.csv", row.names = 1, check.names = FALSE))
weight_control <- as.matrix(read.csv("../fixtures/weights2.csv", row.names=1, check.names = FALSE))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,6], coefficients_control[,6], tolerance = 0.00001)
})

test_that("Seminr estimates the construct scores correctly", {
  expect_equal(construct_scores[,1:6], construct_scores_control[,1:6], tolerance = 0.00001)
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control, tolerance = 0.00001)
})

