# context("SEMinR correctly bootstraps simple models\n")

# Test cases
# ## One antecedent model cases
# set.seed(123)
#
# inn_mm <- constructs(
#   composite("INN_P", multi_items("IMAG", 1:4)),
#   composite("INN_M", multi_items("PERQ", 1:4)),
#   composite("THIRD", multi_items("CUEX", 1:3))
# )
#
# inn_sm <- relationships(paths(from = "INN_P", to = "INN_M"))
# pls <- estimate_pls(data = mobi, inn_mm, inn_sm)
# expect_error(bootstrap_model(seminr_model = pls, nboot = 5), NA)
#
# inn_sm <- relationships(paths(from = "INN_P", to = c("INN_M", "THIRD")))
# pls <- estimate_pls(data = mobi, inn_mm, inn_sm)
# expect_error(bootstrap_model(seminr_model = pls, nboot = 5), NA)


# Test cases
## Simple case
set.seed(123)

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_5_X/boostrapmatrix1.csv")     # V3.5.X
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_6_0/boostrapmatrix1.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(file = paste(test_folder,"boostrapmatrix1.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the interaction model\n")

# Test cases
## Interaction case

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights),
  interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal, weights = mode_A),
  interaction_term(iv = "Image", moderator = "Value", method = orthogonal, weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value",
                 "Image*Expectation", "Image*Value"))
)

# Load data, assemble model, and estimate using semPLS
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$paths_descriptives

## Output originally created using following lines
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_5_X/boostrapmatrix2.csv")     # V3.5.X
# write.csv(bootmodel$paths_descriptives, file = "tests/fixtures/V_3_6_0/boostrapmatrix2.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boostrapmatrix2.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model weights - composite measurement mode\n")

# Test cases
## Regular model

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = correlation_weights),
  composite("Value",        multi_items("PERV", 1:2), weights = correlation_weights),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = correlation_weights)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$weights_descriptives

## Output originally created using following lines
# write.csv(bootmodel$weights_descriptives, file = "tests/fixtures/V_3_5_X/boot_weights.csv")     # V3.5.X
# write.csv(bootmodel$weights_descriptives, file = "tests/fixtures/V_3_6_0/boot_weights.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_weights.csv", sep = ""), row.names = 1))
# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model loadings - factor measurement mode\n")

# Test cases
## Regular model

mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_weighting)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
bootmatrix <- bootmodel$loadings_descriptives

## Output originally created using following lines
# write.csv(bootmodel$loadings_descriptives, file = "tests/fixtures/V_3_5_x/boot_loadings.csv")     # V3.5.X
# write.csv(bootmodel$loadings_descriptives, file = "tests/fixtures/V_3_6_0/boot_loadings.csv")     # V3.6.0


# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_loadings.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly bootstraps the model HTMT - factor measurement mode\n")

# Test cases

# Load outputs
bootmatrix <- bootmodel$HTMT_descriptives

## Output originally created using following lines
# write.csv(bootmodel$HTMT_descriptives, file = "tests/fixtures/V_3_5_X/boot_HTMT.csv")         # V3.5.X
# write.csv(bootmodel$HTMT_descriptives, file = "tests/fixtures/V_3_6_0/boot_HTMT.csv")     # V3.6.0

# Load controls
bootmatrix_control <- as.matrix(read.csv(paste(test_folder,"boot_HTMT.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr performs the bootstrapping correctly", {
  expect_equal(bootmatrix[,1], bootmatrix_control[,1], tolerance = 0.00001)
  expect_equal(bootmatrix[,2], bootmatrix_control[,2], tolerance = 0.00001)
  expect_equal(bootmatrix[,3], bootmatrix_control[,3], tolerance = 0.00001)
})

context("SEMinR correctly reports the confidence interval bootstrapped paths\n")

# Test cases
## Simple case
set.seed(123)
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value")),
  paths(to = "Expectation", from = "Image")
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_factorial)
bootmodel <- bootstrap_model(seminr_model,nboot = 200, cores = 2, seed = 123)

# Load outputs
paths <- bootmodel$boot_paths

## Output originally created using following lines
# write.csv(c(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction"),
#             confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1),
#             confidence_interval(bootmodel, from = "Image", to = "Satisfaction")),
#           file = "tests/fixtures/V_3_6_0/conf_ints.csv")

# Load controls
conf_ints_control <- as.matrix(read.csv(paste(test_folder,"conf_ints.csv", sep = ""), row.names = 1))


# Testing
test_that("Seminr calculates the confidence intervals correctly", {
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[1]],
               conf_ints_control[1,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction")[[2]],
               conf_ints_control[2,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[1]],
               conf_ints_control[3,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", through = "Expectation", to = "Satisfaction", alpha = 0.1)[[2]],
               conf_ints_control[4,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", to = "Satisfaction")[[1]],
               conf_ints_control[5,1], tolerance = 0.00001)
  expect_equal(confidence_interval(bootmodel, from = "Image", to = "Satisfaction")[[2]],
               conf_ints_control[6,1], tolerance = 0.00001)
})
