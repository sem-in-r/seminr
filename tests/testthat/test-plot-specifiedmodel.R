context("plot-basics: specified_model\n")
test_that("dot_graph does not fail for a specified model", {
  require(seminr)
  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Quality",      multi_items("PERQ", 1:7), weights = correlation_weights),
    composite("Value",        multi_items("PERV", 1:2), weights = regression_weights),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    reflective("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
    paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
    paths(from = "Quality",      to = c("Value", "Satisfaction")),
    paths(from = "Value",        to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  model <- specify_model(mobi_mm, mobi_sm)

  model_est <- estimate_pls(data = mobi,
                        model = model)


  #plot(model)
  expect_error(dot_graph(model), NA)
  expect_error(plot(model), NA)


  plot <- plot(model)
  #testthat::expect_true(check_test_plot(plot, title = "Plot with reflectives and composite"))
  #vdiffr::expect_doppelganger(title = "reflectives and composite", fig = plot, writer = write_test)
})

# Cleanup
unlink("Rplots.pdf")
