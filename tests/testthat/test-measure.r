context("Measurement model specification")

# Test cases
## Format 1 (includes all verbs)
mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Quality",      single_item("PERQ1")),
  causal("Satisfaction",    multi_items("CUSA", 1:3)),
  causal("Complaints",      single_item("CUSCO"))
)

# Testing

test_that("constructs correctly specifies the measurement matrix object", {
  expect_equal(colnames(mm), c("latent","measurement","type"))
  expect_equal(nrow(mm), 10)
  expect_equal(ncol(mm), 3)
})

test_that("causal correctly specifies a formative constructs", {
  expect_equal(as.vector(mm[7,]), c("Satisfaction","CUSA1","F"))
  expect_equal(as.vector(mm[10,]), c("Complaints","CUSCO","F"))
})

test_that("reflect correctly specifies a reflective constructs", {
  expect_equal(as.vector(mm[1,]), c( "Image", "IMAG1","R"))
  expect_equal(as.vector(mm[6,]), c("Quality", "PERQ1","R"))
})

test_that("multi_items correctly allocates measurement items", {
  expect_equal(as.vector(mm[1:5,]), c("Image", "Image", "Image", "Image", "Image",
                                      "IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5",
                                      "R","R","R","R","R"))
  expect_equal(as.vector(mm[7:9,]), c("Satisfaction", "Satisfaction", "Satisfaction",
                                      "CUSA1", "CUSA2", "CUSA3",
                                      "F","F","F"))
})

test_that("single_item correctly allocates a measurement item", {
  expect_equal(as.vector(mm[6,]),  c("Quality","PERQ1","R"))
  expect_equal(as.vector(mm[10,]), c("Complaints","CUSCO","F"))
})
