context("CBSEM Specification and estimation\n")
library(seminr)

# Test lavaan_model for cbsem by roundtripping model between seminr <-> lavaan
mm_r <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Complaints",   single_item("CUSCO"))
)

am <- associations(
  item_errors(c("IMAG1", "IMAG2"), "CUEX1"),
  item_errors("CUSA1", "PERV1")
)

sm <- relationships(
  paths(from = c("Image", "Expectation", "Value"), to = "Satisfaction"),
  paths(from = c("Value", "Satisfaction"),         to = "Complaints")
)

seminr_cbsem <- estimate_cbsem(mobi, mm_r, sm)
lav_cbsem <- estimate_cbsem(mobi, lavaan_model = seminr_cbsem$lavaan_model)
seminr_cbsem_report <- summary(seminr_cbsem)
lav_cbsem_report <- summary(lav_cbsem)

test_that("Same lavaan syntax is produced", {
  expect_equal(seminr_cbsem$lavaan_model, lav_cbsem$lavaan_model)
})

test_that("Same seminr structural model and results are produced", {
  expect_true(all(lav_cbsem$smMatrix == seminr_cbsem$smMatrix))
  # expect_true(identical(seminr_cbsem_report$paths, lav_cbsem_report$paths))
})

# Test lavaan_model for cfa by roundtripping model between seminr <-> lavaan

seminr_cfa <- estimate_cfa(mobi, mm_r)
lav_cfa <- estimate_cfa(mobi, lavaan_model = seminr_cfa$lavaan_model)

test_that("CBSEM with measurement structure is estimated", {
  expect_equal(seminr_cfa$lavaan_model, lav_cfa$lavaan_model)
})
