context("CBSEM Specification and estimation\n")
library(seminr)

# Test coercion of measurements to reflective
mm_r <- constructs(
  reflective("Image",       multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Complaints",   single_item("CUSCO"))
)

test_that("Composites can be coerced to be reflective", {
  img_c <- composite("Image",   multi_items("IMAG", 1:5), weights = correlation_weights)
  img_r <- reflective("Image",  multi_items("IMAG", 1:5))
  img_c2r <- as.reflective(img_c)
  expect_true(all(img_r == img_c2r))
})

test_that("Measurement models can be coerced to be reflective", {
  mm_c <- constructs(
    composite("Image",       multi_items("IMAG", 1:5), weights = correlation_weights),
    composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    composite("Value",        multi_items("PERV", 1:2)),
    composite("Complaints",   single_item("CUSCO"))
  )
  mm_c2r <- as.reflective(mm_c)
  expect_true(all(mm_r %in% mm_c2r))
})

# Test estimation of CBSEM through LAVAAN
## Construct names with spaces
sm <- relationships(
  paths(from = c("Image", "Expectation", "Value"), to = "Satisfaction"),
  paths(from = c("Value", "Satisfaction"),         to = "Complaints")
)

cbcfa <- estimate_cfa(mobi, mm_r)

test_that("Measurement model CFA is estimated", {
  expect_true(!is.null(cbcfa))
})

test_that("CFA reports right number of non-zero loadings", {
  expect_equal(sum(cbcfa$factor_loadings != 0), 14)
})

test_that("CBSEM with measurement structure is estimated", {
  cbsem <- estimate_cbsem(mobi, mm_r, sm)
  expect_true(!is.null(cbsem))
})

test_that("CBSEM with measurement structure is estimated", {
  am <- associations(
           item_errors(c("IMAG1", "IMAG2"), "CUEX1"),
           item_errors("CUSA1", "PERV1")
         )
  cbsem <- estimate_cbsem(mobi, mm_r, sm, am)
  expect_true(!is.null(cbsem))
})
