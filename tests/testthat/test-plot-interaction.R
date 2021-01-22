test_that("interaction terms work", {
  set.seed(123)
  mobi <- mobi


  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Quality",      multi_items("PERQ", 1:7)),
    reflective("Loyalty",      multi_items("CUSL", 1:3)),
    interaction_term(iv = "Quality", moderator = "Expectation", method = product_indicator)
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = c("Image", "Quality", "Expectation", "Quality*Expectation"), to = c("Loyalty"))
  )

  model <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)


  model_boot <- bootstrap_model(model, nboot = 100, cores = 1)

  testthat::expect_error(dot_graph(model), NA)
  testthat::expect_error(plot(model), NA)
  testthat::expect_error(plot(model_boot), NA)
})
