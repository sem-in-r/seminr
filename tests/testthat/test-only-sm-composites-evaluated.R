context("SEMinR only evaluates the HOCs and composites in the SM\n")

set.seed(1)
comp1_items <- multi_items("IMAG", 1:5)
comp2_items <- multi_items("CUEX", 1:3)
comp3_items <- multi_items("CUSA", 1:3)

mobi_mm <- constructs(
  # First-order composites
  composite("Comp1", comp1_items),
  composite("Comp2", comp2_items),
  composite("Comp3", comp3_items),

  # First-order representation of higher-order composite (repeated indicators)
  composite("Comp_1_2", c(comp1_items, comp2_items)),

  # Second-order composite of two composites
  higher_composite("HOC_1_2", c("Comp1", "Comp2")),
  higher_composite("HOC_1_3", c("Comp1", "Comp3"))
)


# HAPPY: no duplicate exogenous constructs
happy_sm_lower <- relationships(
  paths(from="Comp3", to="Comp1"),
  paths(from="Comp3", to="Comp2")
)

happy_pls_lower <- estimate_pls(mobi, mobi_mm, happy_sm_lower)
summary_object <- summary(happy_pls_lower)

# Load outputs
output <- capture.output(estimate_pls(mobi, mobi_mm, happy_sm_lower))[1:2]
output_mm <- happy_pls_lower$mmMatrix
output_paths <- summary_object$paths

## Output originally created using following lines
# write.csv(capture.output(estimate_pls(mobi, mobi_mm, happy_sm_lower))[1:2], file = "tests/fixtures/V_3_6_0/hoc_test_output.csv")
# write.csv(capture.output(estimate_pls(mobi, mobi_mm, happy_sm_lower))[1:2], file = "tests/fixtures/V_3_5_X/hoc_test_output.csv")
# write.csv(happy_pls_lower$mmMatrix, file = "tests/fixtures/V_3_6_0/hoc_test_output_mm.csv")
# write.csv(happy_pls_lower$mmMatrix, file = "tests/fixtures/V_3_5_X/hoc_test_output_mm.csv")
# write.csv(summary_object$paths, file = "tests/fixtures/V_3_6_0/hoc_test_output_paths.csv")
# write.csv(summary_object$paths, file = "tests/fixtures/V_3_5_X/hoc_test_output_paths.csv")

# Load controls
output_control <- as.matrix(read.csv(file = paste(test_folder,"hoc_test_output.csv", sep = ""), row.names = 1))
output_mm_control <- as.matrix(read.csv(file = paste(test_folder,"hoc_test_output_mm.csv", sep = ""), row.names = 1))
output_paths_control <- as.matrix(read.csv(file = paste(test_folder,"hoc_test_output_paths.csv", sep = ""), row.names = 1))

# Testing

test_that("Seminr estimates the model once", {
  expect_equal(output, as.vector(output_control), tolerance = 0.00001)
})

test_that("Seminr retains the full measurement model", {
  expect_equal(output_mm[1,], output_mm_control[1,], tolerance = 0.00001)
})

test_that("Seminr correctly summarizes the model", {
  expect_equal(c(round(output_paths,3)[1:3,1],round(output_paths,3)[1:3,2]), c(output_paths_control[1:3,1],output_paths_control[1:3,2]), tolerance = 0.00001)
})
