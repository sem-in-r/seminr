context("Measurement model specification")

# Test cases
## Format 1 (includes all verbs)
mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Quality",      single_item("PERQ1")),
  composite("Satisfaction",  multi_items("CUSA", 1:3),weights = regression_weights),
  composite("Complaints",    single_item("CUSCO"),weights = correlation_weights),
  composite("Value", multi_items("PERV", 1:2), unit_weights)
)

# Testing

test_that("constructs correctly specifies the measurement list object", {
  expect_equal(names(mm), c("reflective", "reflective", "composite",  "composite", "composite"))
  expect_equal(length(mm), 5)
})

test_that("composite correctly specifies mode B constructs", {
  expect_equal(as.vector(mm[[3]][1:3]), c("Satisfaction","CUSA1","B"))
})

test_that("composite correctly specifies unit weight constructs", {
  expect_equal(as.vector(mm[[5]][1:3]), c("Value","PERV1","UNIT"))
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

context("Measurement model correctly estimated")

# Test cases
## Format 1 (includes all verbs)
mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Quality",      single_item("PERQ1")),
  composite("Satisfaction",  multi_items("CUSA", 1:3),weights = regression_weights),
  composite("Complaints",    single_item("CUSCO"),weights = correlation_weights),
  composite("Value",         multi_items("PERV", 1:2), unit_weights)
)
sm <- relationships(
  paths(from = "Image",       to = "Satisfaction"),
  paths(from = "Quality",     to = "Satisfaction"),
  paths(from = "Complaints",  to = "Satisfaction"),
  paths(from = "Value",       to = "Satisfaction")
)

seminr_model <- estimate_pls(data = mobi,
                              measurement_model = mm,
                              structural_model = sm)
# Load fixtures
## Output originally created using following lines
# write.csv(seminr_model$outer_weights, file = "tests/fixtures/V_3_6_0/weights3.csv", row.names=TRUE)
# write.csv(seminr_model$outer_loadings, file = "tests/fixtures/V_3_6_0/loadings3.csv", row.names=TRUE)

# Load controls
weight_control <- as.matrix(read.csv(file = paste(test_folder,"weights3.csv", sep = ""), row.names = 1, check.names = FALSE))
loadings_control <- as.matrix(read.csv(file = paste(test_folder,"loadings3.csv", sep = ""), row.names = 1, check.names = FALSE))

# Testing
test_that("construct relations are accurately estimated", {
  expect_equal(seminr_model$outer_weights,weight_control)
  expect_equal(seminr_model$outer_loadings,loadings_control)
})
