test_that("dot_graph does not fail for cbsem models", {
  if (semPlot_present) {
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

    model_cb <- estimate_cbsem(data = mobi,
                               measurement_model = as.reflective(mobi_mm),
                               structural_model = mobi_sm)

    # now supported

    expect_error(dot_graph(model_cb), NA)


    # Identify any inter-item association parameters to estimate by
    # specifying free associations between their errors
    mobi_am <- associations(
      item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
      item_errors("IMAG1", "CUEX2")
    )

    model_cb <- estimate_cbsem(data = mobi,
                               measurement_model = as.reflective(mobi_mm),
                               structural_model = mobi_sm, item_associations = mobi_am)

    # now supported
    expect_error(dot_graph(model_cb, what = "std"), NA)
    expect_error(plot(model_cb, what = "std"), NA)


    # TODO prevent creation of RPLOTS.pdf
    unlink("Rplots.pdf")
  }
})
