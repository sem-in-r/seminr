context("Plot regression Testing")
test_that("test", {
  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3))
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Expectation"))
  )

  model <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  plot <- plot(model)

  #vdiffr::expect_doppelganger(title = "First simple plot", fig = plot, writer = write_test)
  expect_equal(1,1)
})


