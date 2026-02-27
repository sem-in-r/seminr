context("mmMatrix accessor functions\n")

# Test fixture: build a representative mmMatrix
mm_list <- constructs(
  composite("Image",        multi_items("IMAG", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_B),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Loyalty",      multi_items("CUSL", 1:2))
)
mm <- mm2matrix(mm_list)

# --- all_constructs ---
test_that("all_constructs returns all unique construct names", {
  result <- all_constructs(mm)
  expect_setequal(result, c("Image", "Value", "Satisfaction", "Loyalty"))
})

test_that("all_constructs returns character vector", {
  expect_type(all_constructs(mm), "character")
})

# --- all_constructs_of_mode ---
test_that("all_constructs_of_mode returns constructs matching a mode", {
  expect_setequal(all_constructs_of_mode(mm, "A"), c("Image", "Loyalty"))
  expect_setequal(all_constructs_of_mode(mm, "B"), c("Value"))
  expect_setequal(all_constructs_of_mode(mm, "C"), c("Satisfaction"))
})

test_that("all_constructs_of_mode returns empty vector for non-existent mode", {
  expect_length(all_constructs_of_mode(mm, "UNIT"), 0)
})

# --- construct_of_item ---
test_that("construct_of_item returns the construct containing an item", {
  expect_equal(construct_of_item(mm, "IMAG1"), "Image")
  expect_equal(construct_of_item(mm, "PERV1"), "Value")
  expect_equal(construct_of_item(mm, "CUSA1"), "Satisfaction")
})

# --- is_interaction ---
test_that("is_interaction detects interaction construct names", {
  expect_true(is_interaction("Image*Value"))
  expect_true(is_interaction("A*B"))
})

test_that("is_interaction returns FALSE for non-interaction names", {
  expect_false(is_interaction("Image"))
  expect_false(is_interaction("Satisfaction"))
})

test_that("is_interaction is vectorized", {
  result <- is_interaction(c("Image", "Image*Value", "Satisfaction"))
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

# --- has_interactions (on smMatrix) ---
sm_with_int <- relationships(
  paths(from = c("Image", "Value", "Image*Value"), to = "Satisfaction")
)

sm_no_int <- relationships(
  paths(from = c("Image", "Value"), to = "Satisfaction")
)

test_that("has_interactions detects interaction terms in smMatrix", {
  expect_true(has_interactions(sm_with_int))
  expect_false(has_interactions(sm_no_int))
})

test_that("has_interactions with outcome filters to specific DV", {
  sm_multi <- relationships(
    paths(from = c("Image", "Value", "Image*Value"), to = "Satisfaction"),
    paths(from = c("Image"), to = "Loyalty")
  )
  expect_true(has_interactions(sm_multi, outcome = "Satisfaction"))
  expect_false(has_interactions(sm_multi, outcome = "Loyalty"))
})

# --- construct_names S3 generic ---
test_that("construct_names dispatches on structural_model", {
  sm <- relationships(
    paths(from = c("Image", "Value"), to = "Satisfaction")
  )
  result <- construct_names(sm)
  expect_setequal(result, c("Image", "Value", "Satisfaction"))
})

# --- construct_names on estimated model ---
context("construct_names and construct_scores on estimated models\n")

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)
mobi_sm <- relationships(
  paths(from = c("Image", "Expectation"), to = "Satisfaction"),
  paths(from = "Satisfaction", to = "Loyalty"),
  paths(from = "Image", to = "Value"),
  paths(from = "Value", to = "Satisfaction")
)
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)

test_that("construct_names on pls_model returns model construct names", {
  result <- construct_names(mobi_pls)
  expect_setequal(result, c("Image", "Expectation", "Satisfaction", "Value", "Loyalty"))
})

test_that("construct_scores returns construct score matrix", {
  scores <- construct_scores(mobi_pls)
  expect_true(is.matrix(scores))
  expect_equal(ncol(scores), length(mobi_pls$constructs))
})

test_that("constructs_in_model still works as backward-compatible wrapper", {
  bundle <- constructs_in_model(mobi_pls)
  expect_setequal(bundle$construct_names, construct_names(mobi_pls))
  expect_true(is.matrix(bundle$construct_scores))
  expect_equal(length(bundle$construct_types), length(bundle$construct_names))
})
