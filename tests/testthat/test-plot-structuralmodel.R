test_that("Plotting Structural model", {
  #library(seminr)

  # add complexity?
  structural_model <- relationships(paths("A", "B"))

  #dot_graph.measurement_model(measurement_model, title = "Example plot")
  expect_error(dot_graph(structural_model, title = "Example plot"), NA)
  expect_error(plot(structural_model, title = "Example plot"), NA)

 # plot_model(measurement_model)

  unlink("Rplots.pdf")
  # Todo: RPlots
})
