context("SEMinR dot_graph can plot models that use HOC")
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
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )

  # Creating structural model
  # - note, multiple paths can be created in each line
  mobi_sm <- relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value","Nick"))
  )

  # Estimate the model with the HOC
  model <- estimate_pls(data = mobi,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)
  # FAILS
  testthat::expect_error(dot_graph(model), NA)
  testthat::expect_error(plot(model), NA)

  plot <- plot(model, structure_only = TRUE)

  #vdiffr::expect_doppelganger(title = "Plot HOC", fig = plot, writer = write_test)

  # Testing
  if (FALSE) {
    summary(model)
    plot_model(mobi_mm)
    plot_model(mobi_sm)
    plot_model(model)
    #DiagrammeR::grViz(dot_graph(model))

  }

})



test_that("two higher order composits are plotted", {
  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5), weights = unit_weights),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Quality",      multi_items("PERQ", 1:7)),
    composite("Value",        multi_items("PERV", 1:2)),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    composite("Complaints",   single_item("CUSCO")),
    composite("Loyalty",      multi_items("CUSL", 1:3)),
    higher_composite("Reputation", c("Image", "Expectation")),
    higher_composite("Goodness", c("Quality", "Value"), weights = mode_B)
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Reputation",        to = c("Satisfaction", "Loyalty", "Goodness")),
    paths(from = "Goodness",      to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)



  testthat::expect_error(plot(mobi_pls), NA)
  testthat::expect_error(dot_graph(mobi_pls), NA)

  # Testing
  if (FALSE) {
    summary(mobi_pls)
    plot_model(mobi_mm)
    plot_model(mobi_sm)
    plot_model(mobi_pls)
    #DiagrammeR::grViz(dot_graph(mobi_pls))

  }
})

test_that("higher order composits are plotted despite differing mm and sm constructs", {
  set.seed(123)
  mobi <- mobi
  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Quality",      multi_items("PERQ", 1:5)),
    composite("Loyalty",      multi_items("CUSL", 1:3)),
    composite("Value",        multi_items("PERV", 1:2)),
    higher_composite("Nick", dimensions = c("Quality","Loyalty"), method = two_stage, weights = mode_B),
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )

  # Creating structural model
  # - note, multiple paths can be created in each line
  mobi_sm <- relationships(
    paths(to = "Satisfaction",
          from = c("Expectation", "Value","Nick"))
  )

  # Estimate the model with the HOC
  model <- estimate_pls(data = mobi,
                        measurement_model = mobi_mm,
                        structural_model = mobi_sm)
  # FAILS
  testthat::expect_error(dot_graph(model), NA)
  testthat::expect_error(plot(model), NA)
  plot <- plot(model, structure_only = TRUE)


})
