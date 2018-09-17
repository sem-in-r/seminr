context("SEMinR correctly estimates the model using path weighting scheme\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)


# structural model: note that name of the interactions  should be
#  the names of its two main s joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)


# Load outputs
coefficients <- seminr_model$path_coef
construct_scores <- seminr_model$construct_scores
weight <- seminr_model$outer_weights

## Output originally created using following lines
#write.csv(seminr_model$path_coef, file = "tests/fixtures/inner_weights_coefficients.csv")
#write.csv(seminr_model$construct_scores, file = "tests/fixtures/inner_weights_constructscores.csv")
#write.csv(seminr_model$outer_weights, file = "tests/fixtures/inner_weights_weights.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/inner_weights_coefficients.csv", row.names = 1))
construct_scores_control <- as.matrix(read.csv("../fixtures/inner_weights_constructscores.csv", row.names = 1))
weight_control <- as.matrix(read.csv("../fixtures/inner_weights_weights.csv", row.names=1))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,4], coefficients_control[,4], tolerance = 0.00001)
})

test_that("Seminr estimates the construct scores correctly", {
  expect_equal(construct_scores[,1:4], construct_scores_control[,1:4], tolerance = 0.00001)
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control, tolerance = 0.00001)
})

context("SEMinR correctly estimates the model using factorial scheme\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)


# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '.' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_factorial)


# Load outputs
coefficients <- seminr_model$path_coef
construct_scores <- seminr_model$construct_scores
weight <- seminr_model$outer_weights

## Output originally created using following lines
#write.csv(seminr_model$path_coef, file = "tests/fixtures/factorial_coefficients.csv")
#write.csv(seminr_model$construct_scores, file = "tests/fixtures/factorial_scores.csv")
#write.csv(seminr_model$outer_weights, file = "tests/fixtures/factorial_weights.csv", row.names=TRUE)

# Load controls
coefficients_control <- as.matrix(read.csv("../fixtures/factorial_coefficients.csv", row.names = 1))
construct_scores_control <- as.matrix(read.csv("../fixtures/factorial_scores.csv", row.names = 1))
weight_control <- as.matrix(read.csv("../fixtures/factorial_weights.csv", row.names=1))

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients[,4], coefficients_control[,4], tolerance = 0.00001)
})

test_that("Seminr estimates the  scores correctly", {
  expect_equal(construct_scores[,1:4], construct_scores_control[,1:4], tolerance = 0.00001)
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equal(weight, weight_control, tolerance = 0.00001)
})
