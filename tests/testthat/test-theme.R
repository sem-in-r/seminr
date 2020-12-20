test_that("Basic themes are created without error", {

  expect_error({
    seminr::seminr_theme_default()
    seminr::seminr_theme_smartpls()
  },
               NA)

})
