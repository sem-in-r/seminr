context("Measurement model specification")

# Test cases
## Format 1 (includes all verbs)
mm <- measure(
  reflect("Image",        multi_items("IMAG", 1:5)),
  reflect("Quality",      single_item("PERQ1")),
  form("Satisfaction",    multi_items("CUSA", 1:3)),
  form("Complaints",      single_item("CUSCO"))
)

# Testing

test_that("measure correctly specifies the measurement matrix object", {
  expect_equal(colnames(mm), c("source","target"))
  expect_equal(nrow(mm), 10)
  expect_equal(ncol(mm), 2)
})

test_that("form correctly specifies a formative measure", {
  expect_equal(as.vector(mm[7,]), c("CUSA1","Satisfaction"))
  expect_equal(as.vector(mm[10,]), c("CUSCO","Complaints"))
})

test_that("reflect correctly specifies a formative measure", {
  expect_equal(as.vector(mm[1,]), c( "Image", "IMAG1"))
  expect_equal(as.vector(mm[6,]), c("Quality", "PERQ1"))
})

test_that("multi_items correctly allocates measurement items", {
  expect_equal(as.vector(mm[1:5,]), c("Image", "Image", "Image", "Image", "Image",
                                      "IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"))
  expect_equal(as.vector(mm[7:9,]), c("CUSA1", "CUSA2", "CUSA3",
                                      "Satisfaction", "Satisfaction", "Satisfaction"))
})

test_that("single_item correctly allocates a measurement item", {
  expect_equal(as.vector(mm[6,]),  c("Quality","PERQ1"))
  expect_equal(as.vector(mm[10,]), c("CUSCO","Complaints"))
})

