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
  # Prediction's checks pass on the full sample, but its R-squared is above one
  expect_warning(model <- suppressMessages(estimate_pls(mobi[rows, ], ecsi_reflective_mm, ecsi_sm)),
                 "R-squared outside \\[0, 1\\] \\(Satisfaction = 1.042\\)",
                 class = "seminr_inadmissible_plsc_warning")
  folds <- rep(1:2, each = 25)
  # Fold 2 trains on the first 25 rows
  fold <- in_and_out_sample_predictions(2, folds, model$data, model, predict_DA)
  expect_match(fold$inadmissible, "rho_A is not positive for Value")
  expect_true(all(is.na(fold$PLS_predicted_outsample_item[26:50, ])))
})

# mobi subsamples whose PLSc solutions are inadmissible in different ways
inadmissible_rows <- list(
  several = c(7, 14, 21, 37, 43, 51, 68, 73, 74, 79, 85, 105, 106, 110, 129, 162, 165, 167,
              182, 187, 210, 213, 215, 217, 225),
  not_positive_definite = c(12, 15, 21, 22, 31, 40, 59, 67, 90, 103, 118, 132, 134, 136, 139,
                            144, 148, 150, 159, 168, 175, 179, 187, 194, 207, 211, 216, 218, 220, 242),
  r_squared = c(2, 14, 16, 21, 29, 30, 31, 32, 38, 45, 50, 51, 58, 60, 72, 77, 81, 85, 88, 91,
                95, 102, 110, 115, 120, 126, 127, 132, 134, 136, 137, 139, 140, 142, 148, 149,
                156, 158, 163, 165, 166, 168, 169, 170, 172, 181, 189, 193, 198, 200, 205, 208,
                214, 217, 226, 231, 244, 245, 246, 250)
)
admissible_rows <- c(6, 11, 12, 18, 35, 38, 40, 51, 54, 57, 58, 59, 67, 70, 71, 73, 78, 94, 105,
                     112, 114, 124, 125, 127, 132, 134, 135, 138, 139, 140, 141, 142, 143, 145,
                     146, 158, 161, 162, 167, 168, 174, 179, 183, 186, 189, 195, 200, 206, 209,
                     214, 221, 222, 223, 224, 231, 232, 234, 235, 238, 247)

estimate_ecsi <- function(rows) {
  suppressMessages(estimate_pls(mobi[rows, ], ecsi_reflective_mm, ecsi_sm))
}

test_that("estimate_pls() warns once with every reason a PLSc solution is inadmissible", {
  expect_warning(model <- estimate_ecsi(inadmissible_rows$several),
                 paste0("inadmissible: rho_A outside \\(0, 1\\] .*Value = 1.016.*; ",
                        "standardized loading above one \\(PERV2\\); ",
                        "a disattenuated construct correlation is 1 or more in absolute value; ",
                        "R-squared outside \\[0, 1\\] \\(Expectation = 1.502\\)"),
                 class = "seminr_inadmissible_plsc_warning")
  # The estimates are still returned
  expect_s3_class(model, "pls_model")
  expect_warning(estimate_ecsi(inadmissible_rows$not_positive_definite),
                 "implied construct correlation matrix is not positive definite",
                 class = "seminr_inadmissible_plsc_warning")
  expect_warning(estimate_ecsi(inadmissible_rows$r_squared),
                 "inadmissible: R-squared outside \\[0, 1\\] \\(Satisfaction = 1.033\\)\\.",
                 class = "seminr_inadmissible_plsc_warning")
})

test_that("estimate_pls() does not warn for an admissible PLSc solution", {
  expect_no_warning(estimate_ecsi(admissible_rows))
})

test_that("cross-validation does not repeat the estimation warning for each fold", {
  model <- estimate_ecsi(admissible_rows)
  set.seed(1)
  warnings <- character()
  withCallingHandlers(
    predict_pls(model, technique = predict_DA, noFolds = 5),
    seminr_inadmissible_plsc_warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_length(warnings, 0)
})

test_that("bootstrap_model() drops inadmissible PLSc resamples and reports how many at the end", {
  model <- estimate_ecsi(admissible_rows)
  messages <- character()
  boot <- withCallingHandlers(
    bootstrap_model(model, nboot = 30, cores = 1, seed = 1),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_gt(boot$boots_dropped, 0)
  expect_equal(boot$boots_requested, 30)
  expect_equal(boot$boots + boot$boots_dropped, 30)
  expect_equal(dim(boot$boot_paths)[3], boot$boots)
  # The count is the last message, not one in the middle of the run
  expect_match(utils::tail(messages, 1),
               paste0(boot$boots, " of 30 resamples used\\. ", boot$boots_dropped, " were dropped"))
  printed <- utils::capture.output(print(summary(boot)))
  expect_true(any(grepl(paste0("Bootstrap resamples:  ", boot$boots, " \\(", boot$boots_dropped,
                               " of 30 dropped"), printed)))
})
