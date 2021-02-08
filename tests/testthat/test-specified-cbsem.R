context("CBSEM Specification and estimation\n")
library(seminr)

# Check that regular specification and specified models yield similar structures in CBSEM
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

cbsem <- estimate_cbsem(mobi, mm_r, sm, am)
model = specify_model(measurement_model=mm_r, structural_model=sm, item_associations = am)
sp_cbsem <- estimate_cbsem(mobi, model=model)

test_that("Same seminr structural model and results are produced", {
  expect_true(all(cbsem$smMatrix == sp_cbsem$smMatrix))
  expect_true(all(cbsem$mmMatrix == sp_cbsem$mmMatrix))
  expect_true(all(cbsem$assocations == sp_cbsem$assocations))
  expect_true(all(cbsem$lavaan_model == sp_cbsem$lavaan_model))
})


# Check that regular specification and specified models yield similar structures in CFA
cfa <- estimate_cfa(mobi, mm_r, am)
model = specify_model(measurement_model=mm_r, structural_model=sm, item_associations = am)
sp_cfa <- estimate_cfa(mobi, model=model)

test_that("Same seminr structural model and results are produced", {
  expect_true(all(cfa$lavaan_model == sp_cfa$lavaan_model))
})
