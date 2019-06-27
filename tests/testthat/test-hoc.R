context("SEMinR correctly estimates model for a HOC\n")

# Test cases
## Simple case
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  two_stage_HOC("Satisfaction", c("Image","Value")),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(from = c("Expectation","Quality"),  to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

# Estimate the model with the HOC
seminr_model <- estimate_pls(data = mobi,
                             measurement_model = mobi_mm,
                             interactions = NULL,
                             structural_model = mobi_sm)

# Load outputs
paths <- seminr_model$path_coef
loadings <- seminr_model$outer_loadings
weights <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(paths, file = "tests/fixtures/V_3_6_0/hoc-paths.csv")
# write.csv(loadings, file = "tests/fixtures/V_3_6_0/hoc-loadings.csv")
# write.csv(weights, file = "tests/fixtures/V_3_6_0/hoc-weights.csv")


# Load controls
paths_control <- as.matrix(read.csv(file = paste(test_folder,"hoc-paths.csv", sep = ""), row.names = 1))
loadings_control <- as.matrix(read.csv(file = paste(test_folder,"hoc-loadings.csv", sep = ""), row.names = 1))
weights_control <- as.matrix(read.csv(file = paste(test_folder,"hoc-weights.csv", sep = ""), row.names = 1))


# Testing

test_that("Seminr estimates paths correctly\n", {
  expect_equal(paths, paths_control, tolerance = 0.00001)
})

test_that("Seminr estimates loadings correctly\n", {
  expect_equal(loadings, loadings_control, tolerance = 0.00001)
})

test_that("Seminr estimates weights correctly\n", {
  expect_equal(weights, weights_control, tolerance = 0.00001)
})

context("SEMinR correctly estimates model for a HOC and second_order_interaction\n")

# Test cases
## Simple case
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:5)),
  composite("Loyalty",      multi_items("CUSL", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  two_stage_HOC(construct_name = "Nick", dimensions = c("Quality","Loyalty")),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Interaction constructs must be created after the measurement model is defined.
# We are using the two_stage method as per Henseler & Chin (2010)
mobi_xm <- interactions(
  interaction_2stage("Image", "Expectation")
)

# Creating structural model
# - note, multiple paths can be created in each line
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value","Nick",
                 "Image*Expectation"))
)

# Estimate the model with the HOC
seminr_model <- estimate_pls(data = mobi,
                             measurement_model = mobi_mm,
                             interactions = mobi_xm,
                             structural_model = mobi_sm)

# Load outputs
paths <- seminr_model$path_coef
loadings <- seminr_model$outer_loadings
weights <- seminr_model$outer_weights

## Output originally created using following lines
# write.csv(paths, file = "tests/fixtures/V_3_6_0/hoc_2si-paths.csv")
# write.csv(loadings, file = "tests/fixtures/V_3_6_0/hoc_2si-loadings.csv")
# write.csv(weights, file = "tests/fixtures/V_3_6_0/hoc-_2siweights.csv")


# Load controls
paths_control <- as.matrix(read.csv(file = paste(test_folder,"hoc_2si-paths.csv", sep = ""), row.names = 1))
loadings_control <- as.matrix(read.csv(file = paste(test_folder,"hoc_2si-loadings.csv", sep = ""), row.names = 1))
weights_control <- as.matrix(read.csv(file = paste(test_folder,"hoc-_2siweights.csv", sep = ""), row.names = 1))


# Testing

test_that("Seminr estimates paths correctly\n", {
  expect_equal(as.numeric(paths), as.numeric(paths_control), tolerance = 0.00001)
})

test_that("Seminr estimates loadings correctly\n", {
  expect_equal(as.numeric(loadings), as.numeric(loadings_control), tolerance = 0.00001)
})

test_that("Seminr estimates weights correctly\n", {
  expect_equal(as.numeric(weights), as.numeric(weights_control), tolerance = 0.00001)
})
