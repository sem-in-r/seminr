context("Plotting HTMT Graphs")
test_that("Basic HTMT Plots work", {
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

  model <- estimate_pls(data = mobi,
                        measurement_model = mobi_mm,
                        structural_model = mobi_sm)

  boot_model <- bootstrap_model(model, nboot = 100, cores = 1, seed = 123)


  expect_error(plot_htmt(boot_model, htmt_threshold = 1, use_ci = T, omit_threshold_edges = T), NA)
  pl <- plot_htmt(boot_model, htmt_threshold = 1, use_ci = T, omit_threshold_edges = T)
  #vdiffr::expect_doppelganger("Basic HTMT Plot", pl, writer = write_test)

  unlink("Rplots.pdf")
})
