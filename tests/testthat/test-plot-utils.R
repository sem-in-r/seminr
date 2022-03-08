test_that("glue_dot works", {
  a <- "test"
  testthat::expect_equal(seminr:::glue_dot("<<a>>"), glue::glue("{a}"))
})

test_that("esc_node works", {
  a <- "test"
  testthat::expect_equal(seminr:::esc_node(a), "\"test\"")
})


test_that("pvalr works", {
  testthat::expect_equal(seminr:::pvalr(0.326, digits = 2), "= 0.33")
  testthat::expect_equal(seminr:::pvalr(0.1), "= 0.100")
  testthat::expect_equal(seminr:::pvalr(0.01), "= 0.010")
  testthat::expect_equal(seminr:::pvalr(0.0001), "< 0.001")

  testthat::expect_equal(seminr:::pvalr(0.326, digits = 2), "= 0.33")
  testthat::expect_equal(seminr:::pvalr(0.1, html = T), "= 0.100")
  testthat::expect_equal(seminr:::pvalr(0.01, digits = 5), "= 0.01000")
  testthat::expect_equal(seminr:::pvalr(0.0001, html = T), "&lt; 0.001")
})


test_that("psignr works", {
  testthat::expect_equal(seminr:::psignr(0.325), "")
  testthat::expect_equal(seminr:::psignr(0.1), "")
  testthat::expect_equal(seminr:::psignr(0.01), "*")
  testthat::expect_equal(seminr:::psignr(0.001), "**")
  testthat::expect_equal(seminr:::psignr(0.0001), "***")

  testthat::expect_equal(seminr:::psignr(0.1, html = T), "")
  testthat::expect_equal(seminr:::psignr(0.01, html = T), "*")
  testthat::expect_equal(seminr:::psignr(0.0001, html = T), "***")
})
