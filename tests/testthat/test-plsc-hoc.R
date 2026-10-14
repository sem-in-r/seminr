context("PLSc corrects higher-order composites of common factors (van Riel et al., 2017)\n")

# Oracle: cSEM two-stage PLSc estimates; see fixtures/make_plsc_hoc_oracle.R
hoc_oracle <- readRDS(test_path("fixtures", "plsc_hoc_oracle.rds"))

hoc_sm <- relationships(
  paths(from = "IE",           to = "Satisfaction"),
  paths(from = "Satisfaction", to = "Loyalty")
)
hoc_mm <- function(image, expectation) {
  constructs(
    image,
    expectation,
    higher_composite("IE", c("Image", "Expectation"), weights = mode_B),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
}
hoc_designs <- list(
  factor_locs    = hoc_mm(reflective("Image", multi_items("IMAG", 1:5)),
                          reflective("Expectation", multi_items("CUEX", 1:3))),
  mixed_locs     = hoc_mm(reflective("Image", multi_items("IMAG", 1:5)),
                          composite("Expectation", multi_items("CUEX", 1:3))),
  composite_locs = hoc_mm(composite("Image", multi_items("IMAG", 1:5)),
                          composite("Expectation", multi_items("CUEX", 1:3)))
)

test_that("the reliability of a HOC composite is w' S* w with the LOCs' reliabilities on the diagonal", {
  for (design in names(hoc_designs)) {
    model <- suppressMessages(estimate_pls(mobi, hoc_designs[[design]], hoc_sm))
    locs <- c("Image", "Expectation")
    rho_locs <- plsc_disattenuation(model$first_stage_model)$rho[locs]
    w <- model$outer_weights[locs, "IE"]
    S <- stats::cor(model$first_stage_model$construct_scores[, locs])
    S_star <- S
    diag(S_star) <- rho_locs
    rho_ie <- drop(t(w) %*% S_star %*% w / (t(w) %*% S %*% w))
    rho_sat <- rho_A(model, "Satisfaction")[1, 1]
    r <- stats::cor(model$construct_scores[, "IE"], model$construct_scores[, "Satisfaction"])
    expect_equal(model$path_coef["IE", "Satisfaction"], r / sqrt(rho_ie * rho_sat),
                 tolerance = 1e-10, info = design)
  }
})

test_that("PLSc estimates of HOC models agree with cSEM's two-stage approach", {
  # Stage-1 estimates differ slightly between the packages (e.g. rho_A of
  # Expectation .462 vs .469), so agreement is to about 0.003, not to rounding.
  # Satisfaction -> Loyalty is not compared: cSEM carries the stage-1
  # reliabilities of first-order constructs into stage 2, seminr re-estimates
  # them, a difference unrelated to the HOC correction
  for (design in names(hoc_designs)) {
    model <- suppressMessages(estimate_pls(mobi, hoc_designs[[design]], hoc_sm))
    expect_equal(model$path_coef["IE", "Satisfaction"], hoc_oracle[[design]]$ie_satisfaction,
                 tolerance = 0.005, info = design)
  }
})

test_that("a HOC composite of composite LOCs is not corrected (rho = 1)", {
  model <- suppressMessages(estimate_pls(mobi, hoc_designs$composite_locs, hoc_sm))
  r <- stats::cor(model$construct_scores[, "IE"], model$construct_scores[, "Satisfaction"])
  rho_sat <- rho_A(model, "Satisfaction")[1, 1]
  expect_equal(model$path_coef["IE", "Satisfaction"], r / sqrt(rho_sat), tolerance = 1e-10)
})

test_that("a HOC of common factors is corrected even when no stage-2 construct is reflective", {
  mm <- constructs(
    reflective("Image",       multi_items("IMAG", 1:5)),
    reflective("Expectation", multi_items("CUEX", 1:3)),
    higher_composite("IE", c("Image", "Expectation"), weights = mode_B),
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )
  sm <- relationships(paths(from = "IE", to = "Satisfaction"))
  model <- suppressMessages(estimate_pls(mobi, mm, sm))
  rho_ie <- hoc_composite_reliability(model, "IE")
  expect_lt(rho_ie, 1)
  r <- stats::cor(model$construct_scores[, "IE"], model$construct_scores[, "Satisfaction"])
  expect_equal(model$path_coef["IE", "Satisfaction"], r / sqrt(rho_ie), tolerance = 1e-10)
})

test_that("estimate_pls() stops with a clear message for higher_reflective()", {
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    higher_reflective("IE", c("Image", "Expectation")),
    reflective("Satisfaction", multi_items("CUSA", 1:3))
  )
  sm <- relationships(paths(from = "IE", to = "Satisfaction"))
  expect_error(estimate_pls(mobi, mm, sm),
               "higher_reflective\\(\\) .*estimate_cbsem\\(\\).*higher_composite\\(\\)")
  expect_error(estimate_pls(mobi, model = specify_model(mm, sm)),
               "higher_reflective\\(\\)")
})

test_that("PLSc() on an estimated higher-order model returns it unchanged", {
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    higher_composite("IE", c("Image", "Expectation")),
    reflective("Satisfaction", multi_items("CUSA", 1:3))
  )
  sm <- relationships(paths(from = "IE", to = "Satisfaction"))
  model <- suppressMessages(estimate_pls(mobi, mm, sm))
  expect_message(again <- PLSc(model), "already applied")
  expect_identical(again, model)
})
