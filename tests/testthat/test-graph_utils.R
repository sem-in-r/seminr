test_that("glue_dot works", {
  a <- "test"
  testthat::expect_equal(seminr:::glue_dot("<<a>>"), glue::glue("{a}"))
})

# TODO: esc_node
# pvalr
