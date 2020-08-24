context("SEMinR correctly cleans the data using mean replacement\n")

# Test cases
# Creating measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Creating structural model ----
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)

# Estimating the model
corp_rep_pls_model <- estimate_pls(data              = corp_rep_data,
                                   measurement_model = corp_rep_mm,
                                   structural_model  = corp_rep_sm,
                                   missing_values_action = mean_replacement,
                                   missing_value_ind = "-99")

# Load outputs
path_coef <- corp_rep_pls_model$path_coef

## Output originally created using following lines
# write.csv(path_coef, file = "tests/fixtures/V_3_6_0/corp_rep_simple_paths.csv")
# write.csv(path_coef, file = "tests/fixtures/V_3_5_X/corp_rep_simple_paths.csv")

# Load controls
paths_control <- as.matrix(read.csv(file = paste(test_folder,"corp_rep_simple_paths.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates mean replacement paths correctly\n", {
  expect_equal(path_coef, paths_control, tolerance = 0.00001)
})
