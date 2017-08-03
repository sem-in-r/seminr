context("Structural model specification")

# Test cases
## Format 1
sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value", "Image.Expectation", "Image.Value"))
)

## Format 2
sm2 <- relationships(
  paths(from = "Image",             to = "Satisfaction"),
  paths(from = "Expectation",       to = "Satisfaction"),
  paths(from = "Value",             to = c("Satisfaction")),
  paths(from = "Image.Expectation", to = "Satisfaction"),
  paths(from = "Image.Value",       to = "Satisfaction")
)

# Testing

test_that("Structural model is correctly generated", {
  expect_equal(sm[,1], c("Image","Expectation", "Value", "Image.Expectation", "Image.Value"))
  expect_equal(sm2[,1], c("Image","Expectation", "Value", "Image.Expectation", "Image.Value"))
  expect_equal(sm[,2], c("Satisfaction", "Satisfaction", "Satisfaction", "Satisfaction", "Satisfaction"))
  expect_equal(sm2[,2], c("Satisfaction", "Satisfaction", "Satisfaction", "Satisfaction", "Satisfaction"))

})

test_that("Matrix in correct format", {
  expect_equal(colnames(sm), c("source","target"))
  expect_equal(colnames(sm2), c("source","target"))
  expect_equal(nrow(sm), 5)
  expect_equal(nrow(sm2), 5)
  expect_equal(ncol(sm), 2)
  expect_equal(ncol(sm2), 2)

})

test_that("Two model syntaxes produce same structural model", {
  expect_identical(sm, sm2)
})
