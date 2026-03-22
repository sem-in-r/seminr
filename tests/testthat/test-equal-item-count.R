context("Equal item count constructs (#364)\n")

# Fixture: all four constructs have exactly 4 items each.
# This triggers sapply's matrix simplification in construct_items and all_LOC_items.
mm_equal <- constructs(
  composite("TI",     multi_items("TI_", 2:5)),
  composite("RF",     multi_items("RF_", 1:4)),
  composite("IA",     multi_items("IA_", 1:4)),
  composite("EFFECT", multi_items("EFFNESS_", 1:4))
)

# --- construct_items on measurement_model returns vector, not matrix ---
test_that("construct_items returns character vector when all constructs have equal item counts", {
  result <- construct_items(mm_equal)
  expect_false(is.matrix(result))
  expect_type(result, "character")
  expect_length(result, 16)
})

# --- all_LOC_items returns vector, not matrix ---
test_that("all_LOC_items returns character vector when all constructs have equal item counts", {
  result <- all_LOC_items(mm_equal)
  expect_false(is.matrix(result))
  expect_type(result, "character")
  expect_length(result, 16)
})
