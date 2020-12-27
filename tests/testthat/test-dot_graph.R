test_that("dot_graph does not fail for a demo data set", {
  set.seed(123)
   mobi <- mobi

   #seminr syntax for creating measurement model
   mobi_mm <- constructs(
                reflective("Image",        multi_items("IMAG", 1:5)),
                reflective("Expectation",  multi_items("CUEX", 1:3)),
                reflective("Quality",      multi_items("PERQ", 1:7)),
                reflective("Value",        multi_items("PERV", 1:2)),
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

   mobi_pls <- estimate_pls(data = mobi,
                            measurement_model = mobi_mm,
                            structural_model = mobi_sm)

   # This creates an unwanted Rplots.pdf file? Why?
  expect_error(dot_graph(mobi_pls), NA)
})


test_that("dot_graph can handle both composite measurement types", {
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

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  expect_error(dot_graph(mobi_pls), NA)
})









test_that("seminr_plot does fail for non pls models", {
  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Quality",      multi_items("PERQ", 1:7)),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    composite("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Satisfaction", "Loyalty")),
    paths(from = "Quality",      to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  model_boot <- bootstrap_model(mobi_pls, nboot = 50, cores = 1)

  model_cb <- estimate_cbsem(data = mobi,
                             measurement_model = as.reflective(mobi_mm),
                             structural_model = mobi_sm)

  # not yet supported
  expect_error(dot_graph(model_boot))
  expect_error(dot_graph(model_cb))



  mobi_mm <- constructs(
    reflective("Image",       multi_items("IMAG", 1:5)),
    reflective("Expectation", multi_items("CUEX", 1:3)),
    reflective("Loyalty",     multi_items("CUSL", 1:3)),
    reflective("Value",       multi_items("PERV", 1:2)),
    reflective("Complaints",  single_item("CUSCO"))
  )

  # Identify any inter-item association parameters to estimate by
  # specifying free associations between their errors
  mobi_am <- associations(
    item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
    item_errors("IMAG1", "CUEX2")
  )

  # CONFIRMATORY FACTOR ANALYSIS
  mobi_cfa <- estimate_cfa(mobi, mobi_mm, mobi_am)
  expect_error(dot_graph(mobi_cfa))

  # prevent creation of RPLOTS.pdf
})
