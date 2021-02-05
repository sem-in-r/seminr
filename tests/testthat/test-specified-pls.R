context("SEMinR accepts fully specified model objects\n")

# PLS Test cases -- derived from test-pls.R

## Primary usage: fully specified model

# measurement model:
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal, weights = mode_A),
  interaction_term(iv = "Image", moderator = "Value", method = orthogonal, weights = mode_A)
)

# structural model:
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi

model = specify_model(measurement_model=mobi_mm, structural_model=mobi_sm)
seminr_model <- estimate_pls(mobi, model = model, inner_weights = path_factorial)

# Load outputs
coefficients <- seminr_model$path_coef
construct_scores <- seminr_model$construct_scores
weight <- seminr_model$outer_weights

# Load controls
coefficients_control <- as.matrix(read.csv(file = paste(test_folder,"coefficients.csv", sep = ""), row.names = 1, check.names = FALSE))
construct_scores_control <- as.matrix(read.csv(file = paste(test_folder,"constructscores.csv", sep = ""), row.names = 1, check.names = FALSE))
weight_control <- as.matrix(read.csv(file = paste(test_folder,"weights.csv", sep = ""), row.names = 1, check.names = FALSE))

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

## Alternative Usage: overriding structural model
mobi_sm2 <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

seminr_model2 <- estimate_pls(mobi, model = model, structural_model = mobi_sm2, inner_weights = path_factorial)

coefficients2 <- seminr_model2$path_coef

test_that("Seminr estimates different number of parameters with overriding structural specification", {
  expect_false(ncol(coefficients2) == ncol(coefficients_control))
})

## Alternative Usage: overriding measurement model

mobi_mm2 <- constructs(
  composite("Image",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Expectation",  multi_items("CUSA", 1:2),weights = mode_A),
  composite("Value",        multi_items("IMAG", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUEX", 1:2),weights = mode_A),
  interaction_term(iv = "Image", moderator = "Expectation", method = two_stage, weights = mode_A),
  interaction_term(iv = "Image", moderator = "Value", method = two_stage, weights = mode_A)
)

seminr_model3 <- estimate_pls(mobi, model = model, measurement_model = mobi_mm2, inner_weights = path_factorial)

coefficients3 <- seminr_model3$path_coef

test_that("Seminr estimates different results with overriding measurement specification", {
  expect_false(isTRUE(all.equal(coefficients3, coefficients_control, tolerance=0.01)))
})

## Import from csem syntax usage:

lav_syntax <- '
  # Composite model
  Image <~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation <~ CUEX1 + CUEX2 + CUEX3
  Value  <~ PERV1  + PERV2
  Satisfaction <~ CUSA1 + CUSA2 + CUSA3

  # Structural model
  Satisfaction ~ Image + Expectation + Value
'

# measurement model:
simple_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_B),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_B),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_B),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_B)
)

# structural model:
simple_sm <- relationships(
  paths(from = c("Image", "Expectation", "Value"), to = "Satisfaction")
)

csem_model <- estimate_pls(mobi, model = csem2seminr(lav_syntax))
seminr_model <- estimate_pls(mobi, simple_mm, simple_sm)

test_that("Seminr estimates different results with overriding measurement specification", {
  expect_true(all.equal(csem_model$path_coef, seminr_model$path_coef))
})
