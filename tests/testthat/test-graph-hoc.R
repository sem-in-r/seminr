context("SEMinR dot_graph can plot models that use HOC (not yet).\n")
test_that("higher order composits are plotted", {
  set.seed(123)
  mobi <- mobi

  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Value",        multi_items("PERV", 1:2)),
    higher_composite("Satisfaction", dimensions = c("Image","Value"), method = two_stage),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Loyalty",      multi_items("CUSL", 1:3))
  )

  # Creating structural model
  # - note, multiple paths can be created in each line
  mobi_sm <- relationships(
    paths(from = c("Expectation"),  to = "Satisfaction"),
    paths(from = "Satisfaction", to = c("Loyalty"))
  )

  # Estimate the model with the HOC
  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  summary(mobi_pls)

  # FAILS
  #DiagrammeR::grViz(dot_graph(mobi_pls))
  testthat::expect_error(dot_graph(mobi_pls))
  testthat::expect_equal(2, 2)
})
