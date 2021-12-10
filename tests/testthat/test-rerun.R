context("rerun of estimated models\n")

# SETUP basic model for rerunning
mobi <- mobi

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

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
                         structural_model = mobi_sm,
                         missing = mean_replacement,
                         missing_value = NA)


test_that("rerun of pls_model object recreates same results", {
  mobi_pls2 <- rerun(mobi_pls)
  expect_true(all(mobi_pls2$path_coef == mobi_pls$path_coef))
})

test_that("rerun of pls_model with changed specification creates different results", {
  mobi_pls3 <- rerun(mobi_pls, measurement_model=as.reflective(mobi_mm))
  expect_false(all(mobi_pls3$path_coef == mobi_pls$path_coef))
})
