context("Shiny app tests")

# These tests verify the app can be constructed without error.
# Full end-to-end tests with shinytest2 require a running browser
# and are meant to be run manually or in CI with appropriate setup.

skip_if_not_installed("shiny")
skip_if_not_installed("visNetwork")

test_that("seminr_app creates a shiny.appobj", {
  # seminr_app returns a shiny app object when launch.browser is FALSE
  # We wrap in a tryCatch to handle the fact that shinyApp returns immediately
  app <- seminr_app(launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("seminr_app accepts preloaded data", {
  app <- seminr_app(data = mobi, launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("seminr_app accepts preloaded model", {
  mobi_mm <- constructs(
    reflective("Image",       multi_items("IMAG", 1:5)),
    reflective("Expectation", multi_items("CUEX", 1:3)),
    reflective("Loyalty",     multi_items("CUSL", 1:3))
  )
  mobi_sm <- relationships(
    paths(from = "Image",       to = "Expectation"),
    paths(from = "Expectation", to = "Loyalty")
  )
  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)
  app <- seminr_app(model = mobi_pls, launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

# -- Module unit tests --

test_that("mod_data_ui creates valid UI", {
  ui <- mod_data_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_builder_ui creates valid UI", {
  ui <- mod_builder_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_estimate_ui creates valid UI", {
  ui <- mod_estimate_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_results_ui creates valid UI", {
  ui <- mod_results_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_code_ui creates valid UI", {
  ui <- mod_code_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})
