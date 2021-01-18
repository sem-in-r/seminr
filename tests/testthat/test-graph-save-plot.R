test_that("Saving a plot works", {
  mobi <- mobi

  # seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Quality",      multi_items("PERQ", 1:7)),
    reflective("Value",        multi_items("PERV", 1:2)),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    reflective("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
  # seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
    paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
    paths(from = "Quality",      to = c("Value", "Satisfaction")),
    paths(from = "Value",        to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  # estimate the model
  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  # generate the plot
  p <- plot_model(mobi_pls)

  extensions <- c("pdf", "png", "ps", "svg", "webp") # webp does not work on bioc

  for (ext in extensions) {
    # save to file
    fname <- paste0("testplot.", ext)
    save_plot(fname, p)
    testthat::expect_equal(file.exists(fname), TRUE)
    testthat::expect_gt(file.info(fname)$size, expected = 0)
    unlink(fname)
  }

})
