context("Code generation tests")

# -- Test fixtures --
mobi_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  composite("Value",         multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction")),
  paths(from = "Expectation",  to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = "Satisfaction"),
  paths(from = "Satisfaction", to = "Loyalty")
)

# -- Tests: generate_seminr_code() --

test_that("generate_seminr_code returns a string", {
  code <- generate_seminr_code(mobi_mm, mobi_sm, data_name = "mobi")
  expect_type(code, "character")
  expect_true(nchar(code) > 0)
})

test_that("generated code contains library call", {
  code <- generate_seminr_code(mobi_mm, mobi_sm)
  expect_true(grepl("library\\(seminr\\)", code))
})

test_that("generated code contains all construct names", {
  code <- generate_seminr_code(mobi_mm, mobi_sm)
  for (name in c("Image", "Expectation", "Value", "Satisfaction", "Loyalty")) {
    expect_true(grepl(name, code, fixed = TRUE),
                info = paste("Missing construct:", name))
  }
})

test_that("generated code contains measurement model types", {
  code <- generate_seminr_code(mobi_mm, mobi_sm)
  expect_true(grepl("reflective", code))
  expect_true(grepl("composite", code))
})

test_that("generated code contains paths", {
  code <- generate_seminr_code(mobi_mm, mobi_sm)
  expect_true(grepl("paths\\(from", code))
})

test_that("generated code contains estimation call", {
  code <- generate_seminr_code(mobi_mm, mobi_sm, estimation = "pls")
  expect_true(grepl("estimate_pls", code))

  code_cb <- generate_seminr_code(mobi_mm, mobi_sm, estimation = "cbsem")
  expect_true(grepl("estimate_cbsem", code_cb))
})

test_that("generated code contains bootstrap when requested", {
  code <- generate_seminr_code(mobi_mm, mobi_sm, bootstrap = TRUE)
  expect_true(grepl("bootstrap_model", code))

  code_noboot <- generate_seminr_code(mobi_mm, mobi_sm, bootstrap = FALSE)
  expect_false(grepl("bootstrap_model", code_noboot))
})

test_that("generated code is valid R (parses without error)", {
  code <- generate_seminr_code(mobi_mm, mobi_sm, data_name = "mobi")
  expect_silent(parse(text = code))
})

test_that("generated code uses data_name parameter", {
  code <- generate_seminr_code(mobi_mm, mobi_sm, data_name = "my_data")
  expect_true(grepl("my_data", code, fixed = TRUE))
})

# -- Tests: detect_multi_items_pattern --

test_that("detect_multi_items_pattern finds sequential patterns", {
  items <- c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")
  result <- detect_multi_items_pattern(items)
  expect_true(grepl("multi_items", result))
  expect_true(grepl("IMAG", result))
  expect_true(grepl("1:5", result))
})

test_that("detect_multi_items_pattern returns NULL for non-patterns", {
  items <- c("age", "gender", "income")
  result <- detect_multi_items_pattern(items)
  expect_null(result)
})

test_that("detect_multi_items_pattern handles non-sequential numbers", {
  items <- c("Q1", "Q3", "Q5")
  result <- detect_multi_items_pattern(items)
  expect_true(grepl("multi_items", result))
  expect_true(grepl("c(1, 3, 5)", result, fixed = TRUE))
})

# -- Tests: codegen with mode_B composite --

test_that("code generation handles mode_B composites", {
  mm <- constructs(
    composite("Test", multi_items("T", 1:3), weights = mode_B)
  )
  sm <- relationships(
    paths(from = "Test", to = "Test")
  )
  code <- generate_seminr_code(mm, sm)
  expect_true(grepl("mode_B", code))
})

# -- Tests: codegen_structural_model --

test_that("structural model code groups paths by source", {
  code <- generate_seminr_code(mobi_mm, mobi_sm)
  # Should have paths(from = "Image", to = c("Expectation", "Satisfaction"))
  expect_true(grepl("from = \"Image\"", code))
})
