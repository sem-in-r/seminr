context("SEMinR correctly unstandardizes model elements\n")

# First order model
measurements <- constructs(
  composite("Image",       multi_items("IMAG", 1:5)),
  composite("Expectation", multi_items("CUEX", 1:3)),
  composite("Loyalty",     multi_items("CUSL", 1:3)),
  composite("Complaints",  single_item("CUSCO"))
)

structure <- relationships(
  paths(from = c("Image", "Expectation"), to = c("Complaints", "Loyalty"))
)

simple_pls_model <- estimate_pls(data = mobi, measurements, structure)
unstd_simple <- unstandardize_scores(simple_pls_model)

# correct_unstd_simple <- unstd_simple
# save(correct_unstd_simple, file="tests/fixtures/V_3_6_0/unstandardized.RData")

# Load correct result objects
load("../fixtures/V_3_6_0/unstandardized.RData")

test_that("Unstandardized scores are reproduced as previously", {
  expect_equal(all(unstd_simple == correct_unstd_simple), TRUE)
})

test_that("Unstandardized scores are in correct scale", {
  complaints_unstd <- summary(unstd_simple$Complaints)
  expect_equal(as.numeric(complaints_unstd["Min."]), 1)
  expect_equal(as.numeric(complaints_unstd["Max."]), 10)

  expectation_unstd <- summary(unstd_simple$Expectation)
  expect_gte(as.numeric(expectation_unstd["Min."]), 1)
  expect_equal(as.numeric(expectation_unstd["Max."]), 10)
})
