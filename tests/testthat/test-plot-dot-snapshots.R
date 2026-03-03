# DOT snapshot regression tests for plot output
#
# Uses testthat 3e expect_snapshot() to lock DOT graph output.
# Snapshots are stored in tests/testthat/_snaps/plot-dot-snapshots.md
# and must be committed to version control.
#
# Snapshot workflow:
#   First run (no snapshots exist):
#     devtools::test(filter = "plot-dot-snapshots")
#     -- creates _snaps/plot-dot-snapshots.md with DOT output for each test
#
#   After intentional plot changes (snapshots will mismatch):
#     devtools::test(filter = "plot-dot-snapshots")
#     -- tests fail showing diff between old and new DOT output
#     testthat::snapshot_review("plot-dot-snapshots")
#     -- interactive review of changes (or use snapshot_accept() to accept all)
#
#   Note: expect_snapshot() skips on CRAN by default (cran = FALSE).

local_edition(3)

test_that("DOT output: basic PLS all reflective", {
  set.seed(123)

  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Quality",      multi_items("PERQ", 1:7)),
    reflective("Value",        multi_items("PERV", 1:2)),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    reflective("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )

  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
    paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
    paths(from = "Quality",      to = c("Value", "Satisfaction")),
    paths(from = "Value",        to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  model <- estimate_pls(
    data = mobi,
    measurement_model = mobi_mm,
    structural_model = mobi_sm
  )

  pdf(nullfile())
  on.exit(dev.off(), add = TRUE)
  expect_snapshot(cat(dot_graph(model)))
})

test_that("DOT output: mixed reflective and composite", {
  set.seed(123)

  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",   multi_items("CUEX", 1:3), weights = unit_weights),
    composite("Quality",       multi_items("PERQ", 1:7), weights = correlation_weights),
    composite("Value",         multi_items("PERV", 1:2), weights = regression_weights),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    reflective("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )

  mobi_sm <- relationships(
    paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
    paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
    paths(from = "Quality",      to = c("Value", "Satisfaction")),
    paths(from = "Value",        to = c("Satisfaction")),
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  model <- estimate_pls(
    data = mobi,
    measurement_model = mobi_mm,
    structural_model = mobi_sm
  )

  pdf(nullfile())
  on.exit(dev.off(), add = TRUE)
  expect_snapshot(cat(dot_graph(model)))
})

test_that("DOT output: interaction term", {
  set.seed(123)

  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Quality",      multi_items("PERQ", 1:7)),
    reflective("Loyalty",      multi_items("CUSL", 1:3)),
    interaction_term(iv = "Quality", moderator = "Expectation", method = product_indicator)
  )

  mobi_sm <- relationships(
    paths(from = c("Image", "Quality", "Expectation", "Quality*Expectation"), to = "Loyalty")
  )

  model <- estimate_pls(
    data = mobi,
    measurement_model = mobi_mm,
    structural_model = mobi_sm
  )

  pdf(nullfile())
  on.exit(dev.off(), add = TRUE)
  expect_snapshot(cat(dot_graph(model)))
})

test_that("DOT output: higher-order composite", {
  set.seed(123)

  mobi_mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Quality",      multi_items("PERQ", 1:5)),
    composite("Loyalty",      multi_items("CUSL", 1:3)),
    composite("Value",        multi_items("PERV", 1:2)),
    higher_composite("Nick", dimensions = c("Quality", "Loyalty"), method = two_stage, weights = mode_B),
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )

  mobi_sm <- relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation", "Value", "Nick"))
  )

  model <- estimate_pls(
    data = mobi,
    measurement_model = mobi_mm,
    structural_model = mobi_sm
  )

  pdf(nullfile())
  on.exit(dev.off(), add = TRUE)
  expect_snapshot(cat(dot_graph(model)))
})
