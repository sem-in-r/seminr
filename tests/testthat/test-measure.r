context("Measurement model specification")

# Test cases
## Format 1 (includes all verbs)
mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Quality",      single_item("PERQ1")),
  composite("Satisfaction",  multi_items("CUSA", 1:3),weights = regression_weights),
  composite("Complaints",    single_item("CUSCO"),weights = correlation_weights)
)

# Testing

test_that("constructs correctly specifies the measurement list object", {
  expect_equal(names(mm), c("reflective", "reflective", "composite",  "composite"))
  expect_equal(length(mm), 4)
})

test_that("composite correctly specifies mode B constructs", {
  expect_equal(as.vector(mm[[3]][1:3]), c("Satisfaction","CUSA1","B"))
})

test_that("composite correctly specifies mode A constructs", {
  expect_equal(as.vector(mm[[4]][1:3]), c("Complaints","CUSCO","A"))
})

test_that("reflect correctly specifies a reflective constructs", {
  expect_equal(as.vector(mm[[1]][1:3]), c( "Image", "IMAG1","C"))
  expect_equal(as.vector(mm[[2]][1:3]), c("Quality", "PERQ1","C"))
})

test_that("multi_items correctly allocates measurement items", {
  expect_equal(as.vector(mm[[1]][1:15]), c("Image", "IMAG1", "C", "Image", "IMAG2", "C", "Image", "IMAG3", "C",
                                           "Image", "IMAG4", "C", "Image", "IMAG5", "C"))
  expect_equal(as.vector(mm[[3]][1:9]), c("Satisfaction", "CUSA1", "B", "Satisfaction", "CUSA2", "B", "Satisfaction",
                                          "CUSA3", "B"))
})

test_that("single_item correctly allocates a measurement item", {
  expect_equal(as.vector(mm[[2]][1:3]),  c("Quality","PERQ1","C"))
  expect_equal(as.vector(mm[[4]][1:3]), c("Complaints","CUSCO","A"))
})
