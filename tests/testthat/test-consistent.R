context("SEMinR correctly estimates rho_A for the simple model\n")

# Test cases
## Simple case
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm,inner_weights = path_factorial)


# Load outputs
rho <- rho_A(seminr_model, constructs_in_model(seminr_model)$construct_names)

## Output originally created using following lines
# write.csv(rho, file = "tests/fixtures/rho1.csv")

# Load controls
rho_control <- as.matrix(read.csv(file = paste(test_folder,"rho1.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates rhoA correctly\n", {
  expect_equal(rho, rho_control, tolerance = 0.00001)
})

context("SEMinR correctly estimates rhoA for the interaction model\n")

# Test cases
## Interaction case

# seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal, weights = mode_A),
  interaction_term(iv = "Image", moderator = "Value", method = orthogonal, weights = mode_A)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate using semPLS
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm,inner_weights = path_factorial)

# Load outputs
rho <- rho_A(seminr_model, constructs_in_model(seminr_model)$construct_names)

## Output originally created using following lines
## write.csv(rho, file = "tests/fixtures/rho2.csv")

# Load controls
rho_control <- as.matrix(read.csv(file = paste(test_folder,"rho2.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates rho_A correctly\n", {
  expect_equal(rho, rho_control, tolerance = 0.00001)
})

context("SEMinR correctly estimates PLSc path coefficients, rsquared and loadings for the simple model\n")

# Test cases
## Simple case
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
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm,inner_weights = path_factorial)
# plscModel <- PLSc(seminr_model)

# Load outputs
path_coef <- seminr_model$path_coef
loadings <- seminr_model$outer_loadings
rSquared <- seminr_model$rSquared

## Output originally created using following lines
# write.csv(path_coef, file = "tests/fixtures/path_coef1.csv")
# write.csv(loadings, file = "tests/fixtures/loadings1.csv")
# write.csv(rSquared, file = "tests/fixtures/rsquaredplsc.csv")


# Load controls
path_coef_control <- as.matrix(read.csv(file = paste(test_folder,"path_coef1.csv", sep = ""), row.names = 1))
loadings_control <- as.matrix(read.csv(file = paste(test_folder,"loadings1.csv", sep = ""), row.names = 1))
rSquared_control <- as.matrix(read.csv(file = paste(test_folder,"rsquaredplsc.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates PLSc path coefficients correctly\n", {
  expect_equal(path_coef, path_coef_control, tolerance = 0.00001)
})

test_that("Seminr estimates PLSc loadings correctly\n", {
  expect_equal(loadings[,1:4], loadings_control, tolerance = 0.00001)
})

test_that("Seminr estimates rsquared  correctly\n", {
  # remove BIC for now
  #expect_equal(rSquared, rSquared_control)
  expect_equal(rSquared[1:2,], rSquared_control[1:2,], tolerance = 0.00001)
})

# Inadmissible PLSc solutions ----
# A small mobi subsample (n = 25) on which Value's two items give rho_A < 0
ecsi_reflective_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)
ecsi_sm <- relationships(
  paths(from = "Image",       to = c("Expectation", "Satisfaction")),
  paths(from = "Expectation", to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",     to = c("Value", "Satisfaction")),
  paths(from = "Value",       to = "Satisfaction")
)
negative_rho_rows <- c(10, 16, 28, 33, 36, 40, 74, 78, 86, 87, 88, 108, 109, 129, 134,
                       141, 142, 149, 163, 178, 187, 205, 239, 242, 244)

test_that("PLSc stops when a rho_A is not positive", {
  expect_error(
    suppressMessages(estimate_pls(mobi[negative_rho_rows, ], ecsi_reflective_mm, ecsi_sm)),
    "rho_A is not positive for Value \\(-2.434\\)",
    class = "seminr_inadmissible_plsc"
  )
})

test_that("cross-validation skips a training fold whose rho_A is not positive", {
  # 50 rows: the first 25 give rho_A < 0, the full sample is admissible
  rows <- c(negative_rho_rows, 72, 120, 207, 229, 225, 136, 47, 235, 79, 166, 148, 44, 64,
            196, 59, 84, 18, 173, 145, 30, 223, 82, 96, 137, 57)
  model <- suppressMessages(estimate_pls(mobi[rows, ], ecsi_reflective_mm, ecsi_sm))
  folds <- rep(1:2, each = 25)
  # Fold 2 trains on the first 25 rows
  fold <- in_and_out_sample_predictions(2, folds, model$data, model, predict_DA)
  expect_match(fold$inadmissible, "rho_A is not positive for Value")
  expect_true(all(is.na(fold$PLS_predicted_outsample_item[26:50, ])))
})
