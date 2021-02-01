test_that("Testing different shapes for different constructs", {


  mobi <- mobi

  #seminr syntax for creating measurement model

  # We use the similar names to create similar lenghts
  mobi_mm <- constructs(
    reflective("Expectation1",    multi_items("IMAG", 1:5)),
    composite("Expectation2",      multi_items("CUEX", 1:3), weights = regression_weights),
    composite("Expectation3",      multi_items("PERQ", 1:7), weights = correlation_weights)
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Expectation1",  to = c("Expectation2")),
    paths(from = "Expectation2",  to = c("Expectation3"))
  )

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)



  # creates senseless theme
  thm <- seminr_theme_create(manifest.reflective.shape =  "ellipse",
                             manifest.compositeA.shape =  "hexagon",
                             manifest.compositeB.shape =  "box",
                             construct.reflective.shape = "hexagon",
                             construct.compositeA.shape = "box",
                             construct.compositeB.shape = "ellipse")

  testthat::expect_error(plot(mobi_pls, theme = thm), NA)

  plot(mobi_pls, theme = thm)
  dot_graph(mobi_pls, theme = thm)
  unlink("Rplots.pdf")
})
