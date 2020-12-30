context("SEMinR dot_graph does not fail for a bootstrapped models.\n")
test_that("bootstrapped models work", {

  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    composite("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  mobi_boot <- bootstrap_model(mobi_pls, nboot = 10, cores = 1)

  expect_error(dot_graph(mobi_boot), NA)

  #DiagrammeR::grViz(dot_graph(mobi_boot))

})
