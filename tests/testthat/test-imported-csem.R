## Import from csem syntax usage:

lav_syntax <- '
  # Composite model
  Image <~ IMAG1 + IMAG2 + IMAG3 + IMAG4 + IMAG5
  Expectation <~ CUEX1 + CUEX2 + CUEX3
  Value  <~ PERV1  + PERV2
  Satisfaction <~ CUSA1 + CUSA2 + CUSA3

  # Structural model
  Satisfaction ~ Image + Expectation + Value
'

# measurement model:
simple_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5),weights = mode_B),
  composite("Expectation",  multi_items("CUEX", 1:3),weights = mode_B),
  composite("Value",        multi_items("PERV", 1:2),weights = mode_B),
  composite("Satisfaction", multi_items("CUSA", 1:3),weights = mode_B)
)

# structural model:
simple_sm <- relationships(
  paths(from = c("Image", "Expectation", "Value"), to = "Satisfaction")
)

csem_model <- estimate_pls(mobi, model = csem2seminr(lav_syntax))
seminr_model <- estimate_pls(mobi, simple_mm, simple_sm)

test_that("Seminr estimates different results with overriding measurement specification", {
  expect_true(all.equal(csem_model$path_coef, seminr_model$path_coef))
})
