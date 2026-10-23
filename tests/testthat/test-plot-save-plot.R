test_that("Saving a plot works", {
  if (rsvg_present) {
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
    p <- plot(mobi_pls)

    extensions <- c("pdf", "png", "ps", "svg") #, "webp") # webp does not work on bioc

    for (ext in extensions) {
      # save to file
      fname <- paste0("testplot.", ext)
      save_plot(fname, p)
      testthat::expect_equal(file.exists(fname), TRUE)
      testthat::expect_gt(file.info(fname)$size, expected = 0)
      unlink(fname)
    }
  }
})

test_that("save_plot() saves CB-SEM and CFA plots (#213)", {
  skip_if_not(semPlot_present)
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Value",        multi_items("PERV", 1:2)),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term(iv = "Image", moderator = "Expectation", method = two_stage)
  )
  sm <- relationships(
    paths(from = c("Image", "Expectation", "Image*Expectation"), to = "Value"),
    paths(from = "Value", to = "Satisfaction")
  )
  model_cb <- suppressMessages(estimate_cbsem(mobi, mm, sm))
  model_cfa <- suppressMessages(estimate_cfa(mobi, mm[1:4]))

  extensions <- c("pdf", "png", "ps")
  if (capabilities("cairo")) extensions <- c(extensions, "svg")
  for (model in list(model_cb, model_cfa)) {
    p <- suppressMessages(plot(model))
    expect_s3_class(p, "qgraph")
    for (ext in extensions) {
      fname <- tempfile(fileext = paste0(".", ext))
      save_plot(fname, p)
      expect_true(file.exists(fname))
      expect_gt(file.info(fname)$size, 0)
      unlink(fname)
    }
  }
  unlink("Rplots.pdf")
})

test_that("save_plot() defaults to the last plot, including a CB-SEM plot (#213)", {
  skip_if_not(semPlot_present)
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Satisfaction", multi_items("CUSA", 1:3))
  )
  sm <- relationships(paths(from = "Image", to = "Satisfaction"))
  model_pls <- suppressMessages(estimate_pls(mobi, mm, sm))
  model_cb <- suppressMessages(estimate_cbsem(mobi, mm, sm))
  if (rsvg_present) suppressMessages(plot(model_pls))
  p_cb <- suppressMessages(plot(model_cb))
  # previously the earlier PLS plot was still the last plot, and was saved instead
  expect_identical(last_seminr_plot(), p_cb)
  fname <- tempfile(fileext = ".pdf")
  save_plot(fname)
  expect_gt(file.info(fname)$size, 0)
  unlink(c(fname, "Rplots.pdf"))
})
