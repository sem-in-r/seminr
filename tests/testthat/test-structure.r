context("Structural model specification\n")

# Test cases
## Format 1
sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value", "Image*Expectation", "Image*Value"))
)

## Format 2
sm2 <- relationships(
  paths(from = "Image",             to = "Satisfaction"),
  paths(from = "Expectation",       to = "Satisfaction"),
  paths(from = "Value",             to = c("Satisfaction")),
  paths(from = "Image*Expectation", to = "Satisfaction"),
  paths(from = "Image*Value",       to = "Satisfaction")
)

# Testing

test_that("Structural model is correctly generated", {
  expect_equal(sm[,1], c("Image","Expectation", "Value", "Image*Expectation", "Image*Value"))
  expect_equal(sm2[,1], c("Image","Expectation", "Value", "Image*Expectation", "Image*Value"))
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

context("Structural model specification - constructs with spaces and numeric construct names interaction and regular\n")

# Test cases
##
# Numeric construct names
sm1 <- relationships(
  paths(to = "Satisfaction",
        from = c("1", "Expectation", "Value", "1*Expectation", "1*Value"))
)

## Construct names with spaces
sm2 <- relationships(
  paths(from = "Image",             to = "Satisfaction"),
  paths(from = "Expectation of",       to = "Satisfaction"),
  paths(from = "Value",             to = c("Satisfaction")),
  paths(from = "Image*Expectation of", to = "Satisfaction"),
  paths(from = "Image*Value",       to = "Satisfaction")
)

mm1 <- constructs(
  composite("1",       multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Value",        multi_items("PERV", 1:2))
)
mm2 <- constructs(
  composite("Image",       multi_items("IMAG", 1:5), weights = correlation_weights),
  composite("Expectation of",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Value",        multi_items("PERV", 1:2))
)


# We are using the orthogonalization method as per Henseler & Chin (2010)
xm1 <- interactions(
  interaction_ortho("1", "Expectation"),
  interaction_ortho("1", "Value")
)

xm2 <- interactions(
  interaction_ortho("Image", "Expectation of"),
  interaction_ortho("Image", "Value")
)

seminr_model1 <- estimate_pls(data = mobi,
                             measurement_model = mm1,
                             interactions = xm1,
                             structural_model = sm1)

seminr_model2 <- estimate_pls(data = mobi,
                              measurement_model = mm2,
                              interactions = xm2,
                              structural_model = sm2)

# Testing

test_that("Structural model correctly handles numeric construct names with and without interaction", {
  expect_equal(seminr_model1$constructs, c("1","Expectation","Value","1*Expectation","1*Value","Satisfaction"))
})
test_that("Structural model correctly handles construct names containing space with and without interaction", {
  expect_equal(seminr_model2$constructs, c("Image","Expectation of","Value","Image*Expectation of","Image*Value","Satisfaction"))
})

context("Structural model specification - can summary function handle construct names numeric and including space\n")

sum1 <- summary(seminr_model1)
sum2 <- summary(seminr_model2)

test_that("Summary method correctly handles construct names containing space with and without interaction", {
  expect_equal(round( sum2$vif_antecedents$Satisfaction[[1]], digits = 6), 1.617113)
})
test_that("Summary method correctly handles numeric construct names with and without interaction", {
  expect_equal(round( sum1$vif_antecedents$Satisfaction[[1]], digits = 6), 1.617113)
})

