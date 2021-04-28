context("SEMinR correctly specifies higher-order factors for CBSEM\n")

  # reflective("Value",        multi_items("PERV", 1:2)),
# Test cases
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

hof_cbsem <- estimate_cbsem(data = mobi, measurement_model = mobi_mm, structural_model = mobi_sm)

test_that("Seminr estimates PI interaction paths correctly\n", {
  expect_match(hof_cbsem$lavaan_model, "ImageSat =~ Image \\+ Satisfaction")
})
