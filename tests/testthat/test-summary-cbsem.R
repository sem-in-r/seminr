context("SEMinR has proper summary structure for CBSEM and CFA models")
# TODO: Tests of summary result contents

mobi <- mobi

#seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

#seminr syntax for freeing up item-item covariances
mobi_am <- associations(
  item_errors(c("PERQ1", "PERQ2"), "IMAG1")
)

#seminr syntax for creating structural model
mobi_sm <- relationships(
  paths(from = c("Image", "Quality"), to = c("Value", "Satisfaction")),
  paths(from = c("Value", "Satisfaction"), to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Estimate model and get results
mobi_cbsem <- estimate_cbsem(mobi, mobi_mm, mobi_sm, mobi_am)
cbsem_summary <- summary(mobi_cbsem)

cbsem_summary_tree <- list(
  meta = list(
    seminr = list(pkgname = NULL, version = NULL),
    engine = list(pkgname = NULL, version = NULL, estimator = NULL),
    syntax = NULL,
    call = NULL
  ),
  model = list(item_names = NULL, construct_names = NULL, estimation = NULL),
  descriptives = list(
    correlations = list(items = NULL, constructs = NULL)
  ),
  loadings = list(coefficients = NULL, significance = NULL),
  paths = list(coefficients = NULL, pvalues = NULL, significance = NULL),
  quality = list(
    fit = list(
      all = NULL,
      curated = list(ordinary = NULL, robust = NULL)
    ),
    reliability = NULL,
    antecedent_vifs = list(
      Complaints = NULL,
      Loyalty = NULL,
      Satisfaction = NULL,
      Value = NULL
    )
  )
)

test_that("Summary of CBSEM has proper structure", {
  expect_equal(traverse_names(cbsem_summary),
               traverse_names(cbsem_summary_tree))
})


# CFA Summary
#seminr syntax for creating measurement model
mobi_mm <- constructs(
  reflective("Image",          multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7))
)

#seminr syntax for freeing up item-item covariances
mobi_am <- associations(
  item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
  item_errors("IMAG1", "CUEX2")
)

mobi_cfa <- estimate_cfa(mobi, mobi_mm, mobi_am)
cfa_summary <- summary(mobi_cfa)

cfa_summary_tree <- list(
  meta = list(
    seminr = list(pkgname = NULL, version = NULL),
    engine = list(pkgname = NULL, version = NULL, estimator = NULL),
    syntax = NULL,
    call = NULL
  ),
  model = list(item_names = NULL, construct_names = NULL, estimation = NULL),
  descriptives = list(
    correlations = list(items = NULL, constructs = NULL)
  ),
  loadings = list(coefficients = NULL, significance = NULL),
  quality = list(
    fit = list(
      all = NULL,
      curated = list(ordinary = NULL, robust = NULL)
    ),
    reliability = NULL
  )
)

test_that("Summary of CFA has proper structure", {
  expect_equal(traverse_names(cfa_summary),
               traverse_names(cfa_summary_tree))
})

test_that("Summary of CFA has reliability values", {
  expect_equal(nrow(cfa_summary$quality$reliability), 3)
  expect_true(all(cfa_summary$quality$reliability > 0))
})
