context("SEMinR plotting structural models")
test_that("Plotting Structural model", {
  #library(seminr)

  # add complexity?
  structural_model <- relationships(paths("A", "B"))

  #dot_graph.measurement_model(measurement_model, title = "Example plot")
  expect_error(dot_graph(structural_model, title = "Example plot"), NA)
  expect_error(plot(structural_model, title = "Example plot"), NA)

 # plot_model(measurement_model)


  plot <- plot(structural_model)
  #vdiffr::expect_doppelganger(title = "Plot structural model", fig = plot, writer = write_test)

  unlink("Rplots.pdf")
  # Todo: RPlots
})
