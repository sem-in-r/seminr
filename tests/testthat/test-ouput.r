context("SEMinR correctly estimates the model")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Expectation",  multi_items("CUEX", 1:3)),
  reflect("Value",        multi_items("PERV", 1:2)),
  reflect("Satisfaction", multi_items("CUSA", 1:3))
)

# interaction factors must be created after the measurement model is defined
mobi_xm <- interact(
  interaction_combo("Image", "Expectation"),
  interaction_combo("Image", "Value")
)

# structural model: note that name of the interactions factor should be
#  the names of its two main factors joined by a '.' in between.
mobi_sm <- structure(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image.Expectation", "Image.Value"))
)

# Load data, assemble model, and estimate using semPLS
data("mobi", package = "semPLS")
mobi_pls <- seminr(mobi, mobi_mm, mobi_xm, mobi_sm)

# Load outputs
coefficients <- mobi_pls$coefficients
factor_scores <- mobi_pls$factor_scores
weight <- mobi_pls$outer_weights
write.csv(mobi_pls$outer_weights, file = "weights.csv")
# Load controls
coefficients_control <- read.csv("../fixtures/coefficients.csv")
factor_scores_control <- as.matrix(read.csv("../fixtures/factorscores.csv")[,2:7])
weight_control <- as.matrix(read.csv("../fixtures/weights.csv")[,2:7])

# Testing

test_that("Seminr estimates the loadings and path coefficients correctly", {
  expect_equal(coefficients$Estimate, coefficients_control[,3])
})

test_that("Seminr estimates the factor scores correctly", {
  expect_equivalent(factor_scores[,1:6], factor_scores_control[,1:6])
})

test_that("Seminr estimates the outer weights correctly", {
  expect_equivalent(weight, weight_control)
})

