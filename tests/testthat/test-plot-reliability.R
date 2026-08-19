# Regression test for the reliability plot threshold.
#
# plot.reliability_table() previously drew its reference line at 0.708, which is
# the indicator LOADING threshold (0.708^2 ~ 0.50 explained variance). The
# metrics this plot shows -- Cronbach's alpha, rhoA and rhoC -- are
# construct-level internal consistency reliabilities, judged against 0.70.
# Reported by Marko Sarstedt.

test_that("the reliability plot threshold defaults to 0.70", {
  expect_equal(formals(plot.reliability_table)$threshold, 0.70)
})

test_that("the loading threshold is not hardcoded in the reliability plot", {
  # Guards against the value being written back into the function body, which is
  # how the original defect arose. Checking the body rather than the default
  # catches a hardcoded abline that ignores the argument.
  body_text <- paste(deparse(body(plot.reliability_table)), collapse = " ")
  expect_false(grepl("0.708", body_text, fixed = TRUE))
  expect_true(grepl("h = threshold", body_text, fixed = TRUE))
})

test_that("the reliability plot accepts a caller-supplied threshold", {
  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )
  mobi_sm <- relationships(
    paths(to = "Satisfaction", from = c("Image", "Expectation"))
  )
  model <- estimate_pls(mobi, measurement_model = mobi_mm,
                        structural_model = mobi_sm)
  rel <- summary(model)$reliability

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot(rel))
  expect_silent(plot(rel, threshold = 0.90))
  expect_invisible(plot(rel))
})
