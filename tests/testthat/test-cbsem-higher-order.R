context("SEMinR correctly specifies higher-order factors for CBSEM\n")

## Test Single HOC in a model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  higher_reflective("ImageSat", c("Image", "Satisfaction")),
  reflective("Expectation", multi_items("CUEX", 1:3)),
  reflective("Loyalty",     multi_items("CUSL", 1:3))
)

mobi_sm <- relationships(
  paths(from = c("ImageSat", "Satisfaction", "Expectation"), to="Loyalty")
)

# hof_cba <- estimate_cfa(data = mobi, measurement_model = mobi_mm)
hof_cbsem <- estimate_cbsem(data = mobi, measurement_model = mobi_mm, structural_model = mobi_sm)
hof_cbsem_summary <- summary(hof_cbsem)

test_that("Seminr estimates PI interaction paths correctly\n", {
  expect_match(hof_cbsem$lavaan_model, "ImageSat =~ Image \\+ Satisfaction")
})

test_that("Seminr estimates higher order factor loadings\n", {
  expect_true(all(hof_cbsem$factor_loadings[c("Image", "Satisfaction"), "ImageSat"] > 0))
})

test_that("Seminr estimates zero loadings on non-HOC constructs\n", {
  first_order_measures <- which(rownames(hof_cbsem$factor_loadings) %in% c("Image", "Satisfaction"))
  expect_true(all(hof_cbsem$factor_loadings[-first_order_measures, "ImageSat"] == 0))
})

test_that("Seminr summarizes higher-order factor reliabilities\n", {
  expect_true(all(hof_cbsem_summary$quality$reliability["ImageSat",] > 0))
})

## Test multiple HOCs in a model
mobi_mm2 <- constructs(
  reflective("Image",          multi_items("IMAG", 1:5)),
  reflective("Satisfaction",    multi_items("CUSA", 1:3)),
  reflective("Expectation",     multi_items("CUEX", 1:3)),
  reflective("Loyalty",         multi_items("CUSL", 1:3)),
  reflective("Quality",        multi_items("PERQ", 1:7)),
  reflective("Value",           multi_items("PERV", 1:2)),
  reflective("Complaints",      single_item("CUSCO")),
  higher_reflective("QualExpImg", c("Quality", "Expectation", "Image")),
  higher_reflective("SatComp", c("Satisfaction", "Complaints"))
)

mobi_sm2 <- relationships(
  paths(from = c("SatComp", "Value", "Loyalty"), to=c("QualExpImg"))
)

hof_cbsem2 <- estimate_cbsem(data = mobi, measurement_model = mobi_mm2, structural_model = mobi_sm2, check.gradient = FALSE)

hof_cbsem2_summary <- summary(hof_cbsem2)

# TODO: make identified 2nd order CFA and test loadings, reliabilities

# First HOC of model
test_that("Seminr estimates higher order factor loadings with multiple HOCs (HOC 1)\n", {
  expect_true(all(hof_cbsem2$factor_loadings[c("Quality", "Expectation", "Image"), "QualExpImg"] > 0))
})

test_that("Seminr estimates zero loadings on non-HOC constructs (HOC 1)\n", {
  first_order_measures <- which(rownames(hof_cbsem2$factor_loadings) %in% c("Quality", "Expectation", "Image"))
  expect_true(all(hof_cbsem2$factor_loadings[-first_order_measures, "QualExpImg"] == 0))
})

test_that("Seminr summarizes higher-order factor reliabilities (HOC 1)\n", {
  expect_true(all(hof_cbsem2_summary$quality$reliability["QualExpImg",] > 0))
})

# Second HOC of model
test_that("Seminr estimates higher order factor loadings with multiple HOCs (HOC 2)\n", {
  expect_true(all(hof_cbsem2$factor_loadings[c("Satisfaction", "Complaints"), "SatComp"] > 0))
})

test_that("Seminr estimates zero loadings on non-HOC constructs (HOC 1)\n", {
  first_order_measures <- which(rownames(hof_cbsem2$factor_loadings) %in% c("Satisfaction", "Complaints"))
  expect_true(all(hof_cbsem2$factor_loadings[-first_order_measures, "SatComp"] == 0))
})

test_that("Seminr summarizes higher-order factor reliabilities (HOC 1)\n", {
  expect_true(all(hof_cbsem2_summary$quality$reliability["SatComp",] > 0))
})
