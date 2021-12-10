context("SEMinR correctly estimates PLS-MGA\n")

# Test cases
mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm,
                         missing = mean_replacement,
                         missing_value = NA)

set.seed(798234)
rand_cond <- sample(c(TRUE, FALSE), nrow(mobi), replace = TRUE)
mobi_mga <- estimate_pls_mga(mobi_pls, rand_cond, nboot=100)

## Output originally created using following lines (put in correct fixtures folder)
# fixture_file <- "tests/fixtures/V_3_6_0/pls-mga-mobi-100.RDS"
# file.remove(fixture_file)
# saveRDS(mobi_mga, file = fixture_file)

# Load controls
mga_correct <- readRDS(paste(test_folder, "pls-mga-mobi-100.RDS", sep = ""))

# Testing

test_that("Seminr estimates the correct PLS-MGA p-values", {
  expect_equal(mobi_mga$pls_mga_p, mga_correct$pls_mga_p, tolerance = 0.00001)
})
