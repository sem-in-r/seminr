context("SEMinR plotting measurement models")
test_that("Plotting Measurement model", {
  #library(seminr)

  mobi <- mobi
  # #seminr syntax for creating measurement model
  measurement_model <- constructs(
                reflective("Image",        multi_items("IMAG", 1:5)),
                reflective("Expectation",  multi_items("CUEX", 1:3)),
                reflective("Quality",      multi_items("PERQ", 1:7)),
                reflective("Value",        multi_items("PERV", 1:2)),
                reflective("Satisfaction", multi_items("CUSA", 1:3)),
                reflective("Complaints",   single_item("CUSCO")),
                reflective("Loyalty",      multi_items("CUSL", 1:3))
              )



  #dot_graph.measurement_model(measurement_model, title = "Example plot")
  expect_error(dot_graph(measurement_model, title = "Example plot"), NA)
  expect_error(plot(measurement_model, title = "Example plot"), NA)


  plot <- plot(measurement_model)
  #vdiffr::expect_doppelganger(title = "Plot measurement model", fig = plot, writer = write_test)

  unlink("Rplots.pdf")
  # Todo: RPlots
})
