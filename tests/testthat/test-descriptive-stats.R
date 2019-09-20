context("SEMinR correctly returns the descriptive statistics in the summary\n")

set.seed(1)
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_A)
)

mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm,inner_weights = path_weighting)
summary_object <- summary(seminr_model)

# Load outputs
# Remove HTMT
#htmt <- summary_object$metrics$Validity$HTMT
item_stats <- summary_object$descriptive_statistics$item_descriptives
item_cors <- summary_object$descriptive_statistics$item_correlations
construct_stats <- summary_object$descriptive_statistics$construct_descriptives
construct_cors <- summary_object$descriptive_statistics$construct_correlations

## Output originally created using following lines
# write.csv(summary_object$descriptive_statistics$item_descriptives, file = "tests/fixtures/V_3_5_X/item_stats.csv")
# write.csv(summary_object$descriptive_statistics$item_descriptives, file = "tests/fixtures/V_3_6_0/item_stats.csv")
# write.csv(summary_object$descriptive_statistics$item_correlations, file = "tests/fixtures/V_3_5_X/item_cors.csv")
# write.csv(summary_object$descriptive_statistics$item_correlations, file = "tests/fixtures/V_3_6_0/item_cors.csv")
# write.csv(summary_object$descriptive_statistics$construct_descriptives, file = "tests/fixtures/V_3_5_X/construct_stats.csv")
# write.csv(summary_object$descriptive_stats$construct_descriptives, file = "tests/fixtures/V_3_6_0/construct_stats.csv")
# write.csv(summary_object$descriptive_statistics$construct_correlations, file = "tests/fixtures/V_3_5_X/construct_cors.csv")
# write.csv(summary_object$descriptive_stats$construct_correlations, file = "tests/fixtures/V_3_6_0/construct_cors.csv")

# load fixtures
item_stats_control <- as.matrix(read.csv(file = paste(test_folder,"item_stats.csv", sep = ""), row.names = 1))
item_cors_control <- as.matrix(read.csv(file = paste(test_folder,"item_cors.csv", sep = ""), row.names = 1))
construct_stats_control <- as.matrix(read.csv(file = paste(test_folder,"construct_stats.csv", sep = ""), row.names = 1))
construct_cors_control <- as.matrix(read.csv(file = paste(test_folder,"construct_cors.csv", sep = ""), row.names = 1))

# Testing
test_that("Seminr estimates the item descriptives correctly", {
  expect_equal(item_stats[1:24, 1:9], item_stats_control[1:24, 1:9], tolerance = 0.00001)
})

test_that("Seminr estimates the item correlations correctly", {
  expect_equal(item_cors[1:24, 1:24], item_cors_control[1:24, 1:24], tolerance = 0.00001)
})

test_that("Seminr estimates the construct descriptives correctly", {
  expect_equal(construct_stats[1:4, 1:9], construct_stats_control[1:4, 1:9], tolerance = 0.00001)
})

test_that("Seminr estimates the construct correlations correctly", {
  expect_equal(construct_cors[1:4, 1:4], construct_cors_control[1:4, 1:4], tolerance = 0.00001)
})

