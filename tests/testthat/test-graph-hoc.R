context("SEMinR dot_graph can plot models that use HOC (not yet).\n")
test_that("higher order composits are plotted", {
  set.seed(123)
  mobi <- mobi
  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Quality",      multi_items("PERQ", 1:5)),
    composite("Loyalty",      multi_items("CUSL", 1:3)),
    composite("Value",        multi_items("PERV", 1:2)),
    higher_composite("Nick", dimensions = c("Quality","Loyalty"), method = two_stage, weights = mode_B),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term(iv = "Image", moderator = "Expectation", method = two_stage, weights = mode_A)
  )

  # Creating structural model
  # - note, multiple paths can be created in each line
  mobi_sm <- relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value","Nick",
                   "Image*Expectation"))
  )

  # Estimate the model with the HOC
  mobi_pls <- estimate_pls(data = mobi,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)

  if (FALSE) {
    summary(mobi_pls)
    plot_model(mobi_mm)
    plot_model(mobi_sm)
    plot_model(mobi_pls)
    #DiagrammeR::grViz(dot_graph(mobi_pls))

  }
  # FAILS
  testthat::expect_error(dot_graph(mobi_pls), NA)
})
