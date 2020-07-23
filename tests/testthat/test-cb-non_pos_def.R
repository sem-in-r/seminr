context("SEMinR catches not-positive-definite errors for CBSEM and CFA models")

mobi <- mobi

mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

mobi_am <- associations(
             item_errors(c("PERQ1", "PERQ2"), "CUEX3"),
             item_errors("IMAG1", "CUEX2")
           )

# CFA
test_that("Seminr catches CFA errors in Lavaan", {
  expect_error(estimate_cfa(mobi, mobi_mm, mobi_am))
})


# CBSEM
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

test_that("Seminr does not erroneously detect CBSEM error in Lavaan", {
  expect_error(estimate_cbsem(mobi, mobi_mm, mobi_sm, mobi_am), 
               NA)
})
