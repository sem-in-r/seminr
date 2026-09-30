context("SEMinR predicts CB-SEM models with the model-implied rule (#426)\n")

# Oracle: lavaan::lavPredictY(), the conditional-mean rule of de Rooij et al.
# (2023), applied to the lavaan fit that estimate_cbsem() produced.
train <- mobi[1:200, ]
test  <- mobi[201:250, ]

chain_mm <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Satisfaction", multi_items("CUSA", 1:3))
)
chain_sm <- relationships(
  paths(from = "Image",       to = "Expectation"),
  paths(from = "Expectation", to = "Quality"),
  paths(from = c("Image", "Expectation", "Quality"), to = "Satisfaction")
)
items_of <- function(model, constructs) {
  unlist(lapply(constructs, function(x) construct_items(model$mmMatrix, x)))
}
oracle_predict <- function(model, x_constructs, y_construct) {
  lavaan::lavPredictY(model$lavaan_output, newdata = test,
                      ynames = items_of(model, y_construct),
                      xnames = items_of(model, x_constructs))
}

test_that("CB-SEM DA predictions equal lavPredictY on each construct's direct antecedents", {
  model <- suppressMessages(estimate_cbsem(train, chain_mm, chain_sm))
  pred <- predict(model, test, technique = predict_DA)
  expect_s3_class(pred, "predicted_seminr_model")
  for (y in c("Expectation", "Quality", "Satisfaction")) {
    o <- oracle_predict(model, construct_antecedents(model$smMatrix, y), y)
    expect_equal(unname(as.matrix(pred$predicted_items[, colnames(o)])), unname(unclass(o)),
                 tolerance = 1e-8, info = y)
  }
})

test_that("CB-SEM EA predictions equal lavPredictY on the exogenous constructs' items", {
  model <- suppressMessages(estimate_cbsem(train, chain_mm, chain_sm))
  pred <- predict(model, test, technique = predict_EA)
  o <- oracle_predict(model, "Image", c("Expectation", "Quality", "Satisfaction"))
  expect_equal(unname(as.matrix(pred$predicted_items[, colnames(o)])), unname(unclass(o)),
               tolerance = 1e-8)
})

test_that("CB-SEM predictions respect item associations (correlated errors)", {
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Satisfaction", multi_items("CUSA", 1:3))
  )
  sm <- relationships(paths(from = "Image", to = "Satisfaction"))
  assoc <- associations(item_errors("IMAG1", "CUSA1"))
  model <- suppressMessages(estimate_cbsem(train, mm, sm, item_associations = assoc))
  pred <- predict(model, test)
  o <- oracle_predict(model, "Image", "Satisfaction")
  expect_equal(unname(as.matrix(pred$predicted_items[, colnames(o)])), unname(unclass(o)),
               tolerance = 1e-8)
})

test_that("CB-SEM residuals are actual minus predicted on the raw item scale", {
  model <- suppressMessages(estimate_cbsem(train, chain_mm, chain_sm))
  pred <- predict(model, test)
  y <- items_of(model, "Satisfaction")
  expect_equal(as.matrix(pred$item_residuals[, y]),
               as.matrix(test[, y]) - as.matrix(pred$predicted_items[, y]),
               ignore_attr = TRUE)
})

test_that("CB-SEM construct predictions are latent conditional expectations", {
  model <- suppressMessages(estimate_cbsem(train, chain_mm, chain_sm))
  pred <- predict(model, test, technique = predict_DA)
  S <- lavaan::lavInspect(model$lavaan_output, "cov.all")
  x <- items_of(model, construct_antecedents(model$smMatrix, "Satisfaction"))
  mu <- colMeans(lavaan::lavInspect(model$lavaan_output, "data"))
  names(mu) <- lavaan::lavNames(model$lavaan_output, "ov")
  X <- sweep(as.matrix(test[, x]), 2, mu[x])
  expected <- X %*% solve(S[x, x], S[x, "Satisfaction"])
  expect_equal(unname(pred$predicted_construct_scores[, "Satisfaction"]),
               unname(drop(expected)), tolerance = 1e-8)
})

test_that("CB-SEM models with interaction terms are refused", {
  mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term(iv = "Image", moderator = "Expectation", method = product_indicator)
  )
  sm <- relationships(
    paths(from = c("Image", "Expectation", "Image*Expectation"), to = "Satisfaction")
  )
  model <- suppressMessages(estimate_cbsem(train, mm, sm))
  expect_error(predict(model, test), "interaction.*427")
})

test_that("predict_pls() still refuses CB-SEM models and points to predict()", {
  model <- suppressMessages(estimate_cbsem(train, chain_mm, chain_sm))
  expect_error(predict_pls(model, noFolds = 5), "CB-SEM.*predict\\(\\)")
})
