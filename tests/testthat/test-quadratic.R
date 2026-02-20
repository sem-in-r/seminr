context("SEMinR correctly estimates quadratic interaction terms (X*X)\n")

# Quadratic model using mobi data: minimal SM with one non-interaction path
# This triggers the drop=FALSE bug when two_stage() filters out interaction rows

# -- Measurement model --
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = "Image", method = two_stage)
)

# -- Structural model (only 1 non-interaction path) --
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Image*Image"))
)

# -- Fix A: two_stage quadratic estimation --
test_that("two_stage quadratic (X*X) estimates without error", {
  expect_no_error(
    estimate_pls(data = mobi,
                 measurement_model = mobi_mm,
                 structural_model = mobi_sm)
  )
})

# -- Fix A (orthogonal): baseline/regression guard --
mobi_mm_ortho <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = "Image", method = orthogonal)
)

test_that("orthogonal quadratic (X*X) estimates without error", {
  expect_no_error(
    estimate_pls(data = mobi,
                 measurement_model = mobi_mm_ortho,
                 structural_model = mobi_sm)
  )
})

# -- Fix A (product indicator): baseline/regression guard --
mobi_mm_pi <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  interaction_term(iv = "Image", moderator = "Image", method = product_indicator)
)

test_that("product_indicator quadratic (X*X) estimates without error", {
  expect_no_error(
    estimate_pls(data = mobi,
                 measurement_model = mobi_mm_pi,
                 structural_model = mobi_sm)
  )
})

# -- Fix A (bootstrap): bootstrap calls two_stage() each iteration --
test_that("bootstrap of two_stage quadratic (X*X) works without error", {
  pls_model <- estimate_pls(data = mobi,
                            measurement_model = mobi_mm,
                            structural_model = mobi_sm)

  expect_no_error(
    bootstrap_model(seminr_model = pls_model,
                    nboot = 50,
                    cores = 1)
  )
})

# -- Fix B: PLSpredict on two_stage quadratic model --
test_that("predict_pls works on two_stage quadratic model", {
  pls_model <- estimate_pls(data = mobi,
                            measurement_model = mobi_mm,
                            structural_model = mobi_sm)

  expect_no_error(
    predict_pls(model = pls_model,
                technique = predict_DA,
                noFolds = 3)
  )
})

# -- quadratic_term() convenience function --
context("quadratic_term() convenience function\n")

test_that("quadratic_term() produces same model as interaction_term(X, X)", {
  mm_quadratic <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    quadratic_term(iv = "Image", method = two_stage)
  )

  model_quadratic <- estimate_pls(data = mobi,
                                  measurement_model = mm_quadratic,
                                  structural_model = mobi_sm)

  model_interaction <- estimate_pls(data = mobi,
                                    measurement_model = mobi_mm,
                                    structural_model = mobi_sm)

  expect_equal(model_quadratic$path_coef, model_interaction$path_coef)
  expect_equal(model_quadratic$outer_loadings, model_interaction$outer_loadings)
})
